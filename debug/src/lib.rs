use std::collections::HashMap;

use proc_macro::TokenStream;

use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Comma, Eq};
use syn::visit::{self, Visit};
use syn::Fields;
use syn::Ident;
use syn::TypePath;
use syn::{
    parse_macro_input, parse_quote, Attribute, DataStruct, DeriveInput, Expr, ExprLit, Field,
    FieldsNamed, GenericParam, Generics, Lit, Meta, MetaNameValue, Type,
};
use syn::{Data, LitStr};

use quote::quote;

struct TypePathVisitor {
    generic_type_params: Vec<Ident>,
    associated_types: HashMap<Ident, Vec<TypePath>>,
}

impl<'a> Visit<'a> for TypePathVisitor {
    fn visit_type_path(&mut self, ty: &'a TypePath) {
        if ty.path.segments.len() >= 2 {
            let generic_type_param = &ty.path.segments[0].ident;
            if self.generic_type_params.contains(generic_type_param) {
                self.associated_types
                    .entry(generic_type_param.clone())
                    .or_default()
                    .push(ty.clone());
            }
        }

        visit::visit_type_path(self, ty);
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn custom_debug(item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let name = &ast.ident;

    let fields = match &ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => named,
        _ => {
            return syn::Error::new(ast.span(), "only works for struct with named fields")
                .to_compile_error()
                .into()
        }
    };

    let debug_fields: syn::Result<Vec<proc_macro2::TokenStream>> = fields
        .iter()
        .map(|f| {
            let field_name = &f.ident;
            let field_value = extract_attribute_from_field(f, "debug")
                .map(|a| &a.meta)
                .map(|m| match m {
                    Meta::NameValue(MetaNameValue {
                        value:
                            Expr::Lit(ExprLit {
                                lit: Lit::Str(litearl_string),
                                ..
                            }),
                        ..
                    }) => Ok(quote! {
                        &format_args!(#litearl_string, &self.#field_name)
                    }),

                    _ => Err(syn::Error::new(
                        m.span(),
                        "expected key and value for debug attribute",
                    )),
                })
                .transpose()?
                .unwrap_or_else(|| {
                    quote! {
                        &self.#field_name
                    }
                });

            Ok(quote! {
                field(stringify!(#field_name), #field_value)
            })
        })
        .collect();

    let debug_fields = match debug_fields {
        Ok(fields) => fields,
        Err(err) => return err.to_compile_error().into(),
    };

    let attr = extract_trait_bound_from_attributes(&ast.attrs);

    let mut generics;
    if let Some(attr) = attr {
        generics = ast.generics;
        generics.make_where_clause();
        generics
            .where_clause
            .as_mut()
            .unwrap()
            .predicates
            .push(syn::parse_str(&attr).unwrap());
    } else {
        let associated_types = generic_associated_types(&ast);
        generics = add_trait_bounds(ast.generics, fields, &associated_types);

        generics.make_where_clause();
        associated_types.values().for_each(|associated_types| {
            associated_types.iter().for_each(|associated_type| {
                generics
                    .where_clause
                    .as_mut()
                    .unwrap()
                    .predicates
                    .push(parse_quote! {
                        #associated_type: std::fmt::Debug
                    })
            })
        });
    }

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    quote! {
        impl #impl_generics std::fmt::Debug for #name #type_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#name))
                #(.#debug_fields)*
                .finish()
            }
        }
    }
    .into()
}

fn add_trait_bounds(
    mut generics: Generics,
    fields: &Punctuated<Field, Comma>,
    associated_types: &HashMap<Ident, Vec<TypePath>>,
) -> Generics {
    let field_type_names = fields
        .iter()
        .flat_map(get_field_type_name)
        .cloned()
        .collect::<Vec<_>>();

    let phantomdata_type_param_names = fields
        .iter()
        .flat_map(phantomdata_inner_type)
        .cloned()
        .collect::<Vec<_>>();

    generics.params.iter_mut().for_each(|param| {
        if let GenericParam::Type(type_param) = param {
            if (phantomdata_type_param_names.contains(&type_param.ident)
                || associated_types.contains_key(&type_param.ident))
                && !field_type_names.contains(&type_param.ident)
            {
            } else {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    });

    generics
}

fn generic_associated_types(ast: &DeriveInput) -> HashMap<Ident, Vec<TypePath>> {
    let generic_type_params = ast
        .generics
        .params
        .iter()
        .filter_map(|param| match param {
            GenericParam::Type(ty) => Some(ty.ident.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut visitor = TypePathVisitor {
        generic_type_params,
        associated_types: HashMap::new(),
    };

    visitor.visit_derive_input(ast);
    visitor.associated_types
}

fn get_field_type_name(f: &Field) -> Option<&Ident> {
    match &f.ty {
        Type::Path(TypePath { path, .. }) if path.segments.last().is_some() => {
            Some(&path.segments.last().unwrap().ident)
        }
        _ => None,
    }
}

use syn::{GenericArgument, PathArguments};

fn phantomdata_inner_type(f: &Field) -> Option<&Ident> {
    let path = match &f.ty {
        Type::Path(ty) => &ty.path,
        _ => return None,
    };

    if path.leading_colon.is_some() {
        return None;
    }

    if path.segments.len() != 1 || path.segments[0].ident != "PhantomData" {
        return None;
    }

    let ab = match &path.segments[0].arguments {
        PathArguments::AngleBracketed(ab) => ab,
        _ => return None,
    };

    if ab.args.len() != 1 {
        return None;
    }

    match &ab.args[0] {
        GenericArgument::Type(Type::Path(ty)) if ty.path.segments.len() == 1 => {
            Some(&ty.path.segments[0].ident)
        }
        _ => None,
    }
}

fn extract_trait_bound_from_attributes(attrs: &[Attribute]) -> Option<String> {
    attrs
        .iter()
        .filter(|attr| attr.path().is_ident("debug"))
        .find_map(|attr| {
            let mut content = None;
            attr.parse_nested_meta(|m| {
                if m.path.is_ident("bound") {
                    let _: Eq = m.input.parse().unwrap();
                    content = Some(m.input.parse::<LitStr>().unwrap().value())
                }
                Ok(())
            })
            .unwrap();

            content
        })
}

fn extract_attribute_from_field<'a>(f: &'a Field, name: &str) -> Option<&'a Attribute> {
    f.attrs.iter().find(|attr| attr.path().is_ident(name))
}
