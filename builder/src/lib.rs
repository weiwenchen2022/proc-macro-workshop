use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

use quote::{format_ident, quote};

use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Comma, Eq};
use syn::Data;
use syn::Fields;
use syn::{
    parse_macro_input, Attribute, DataStruct, DeriveInput, Field, FieldsNamed, Ident, LitStr, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder(item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let name = &ast.ident;
    let builder = format_ident!("{}Builder", name);

    let fields = match &ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => named,

        _ => {
            return syn::Error::new(ast.span(), "only works for structs with named fields")
                .to_compile_error()
                .into()
        }
    };

    let builder_fields = builder_field_definitions(fields);

    let builder_inits = builder_init_values(fields);

    let builder_methods = match builder_methods(fields) {
        Ok(methods) => methods,
        Err(err) => return err.to_compile_error().into(),
    };

    let original_struct_set_fields = original_struct_setter(fields);

    quote! {
        pub struct #builder {
            #(#builder_fields,)*
        }

        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #(#builder_inits,)*
                }
            }
        }

        impl #builder {
            #(#builder_methods)*

            pub fn build(&mut self) -> core::result::Result<#name, std::boxed::Box<dyn std::error::Error + Send + Sync + 'static>> {
                std::result::Result::Ok(#name {
                    #(#original_struct_set_fields,)*
                })
            }
        }
    }
    .into()
}

fn builder_field_definitions(
    fields: &Punctuated<Field, Comma>,
) -> impl Iterator<Item = TokenStream2> + '_ {
    fields.iter().map(|f| {
        let (field_name, field_type) = get_name_and_type(f);

        let field_type = if option_inner_type(field_type).is_some() {
            quote! {
                #field_type
            }
        } else {
            quote! {
                core::option::Option<#field_type>
            }
        };

        quote! {
            #field_name: #field_type
        }
    })
}

fn builder_init_values(
    fields: &Punctuated<Field, Comma>,
) -> impl Iterator<Item = TokenStream2> + '_ {
    fields.iter().map(|f| {
        let (field_name, _) = get_name_and_type(f);
        quote! {
            #field_name: core::option::Option::None
        }
    })
}

fn builder_methods(fields: &Punctuated<Field, Comma>) -> syn::Result<Vec<TokenStream2>> {
    fields
        .iter()
        .map(|f| {
            let (field_name, field_type) = get_name_and_type(f);

            let attr = extract_attribute_from_field(f, "builder")
                .map(|a| {
                    let mut content = None;
                    a.parse_nested_meta(|m| {
                        if !m.path.is_ident("each") {
                            return Err(syn::Error::new_spanned(
                                &a.meta,
                                r#"expected `builder(each = "...")`"#,
                            ));
                        }

                        let _: Eq = m.input.parse().unwrap();
                        let value: LitStr = m.input.parse().unwrap();
                        content = Some(Ident::new(&value.value(), value.span()));
                        Ok(())
                    })?;

                    Ok::<_, syn::Error>(content.unwrap())
                })
                .transpose()?;

            let each_method = if let Some(each_method_name) = attr.as_ref() {
                if let Some(inner_type) = generic_inner_type(field_type, "Vec") {
                    quote! {
                        pub fn #each_method_name(&mut self, input: #inner_type) -> &mut Self {
                            match self.#field_name.as_mut() {
                                core::option::Option::Some(#field_name) => #field_name.push(input),
                                core::option::Option::None => self.#field_name = core::option::Option::Some(vec![input]),
                            }
                            self
                        }
                    }
                } else {
                    quote! {}
                }
            } else {
                quote! {}
            };

            let field_method = if attr
                .as_ref()
                .map(|attr| attr == field_name)
                .unwrap_or(false)
            {
                quote! {}
            } else {
                let field_type = if let Some(inner_type) = option_inner_type(field_type)
                {
                    quote! {
                        #inner_type
                    }
                } else {
                    quote! {
                        #field_type
                    }
                };

                quote! {
                    fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                        self.#field_name = core::option::Option::Some(#field_name);
                        self
                    }
                }
            };

            Ok(quote! {
                #each_method

                #field_method
            })
        })
        .collect()
}

fn extract_attribute_from_field<'a>(f: &'a Field, name: &str) -> Option<&'a Attribute> {
    f.attrs.iter().find(|attr| attr.path().is_ident(name))
}

fn original_struct_setter(
    fields: &Punctuated<Field, Comma>,
) -> impl Iterator<Item = TokenStream2> + '_ {
    fields.iter().map(|f| {
        let (field_name, field_type) = get_name_and_type(f);

        let handler = if option_inner_type(field_type).is_some() {
            quote! {
                take()
            }
        } else if generic_inner_type(field_type, "Vec").is_some() {
            quote! {
                take().unwrap_or_default()
            }
        } else {
            quote! {
                take().ok_or_else(|| format!("field '{}' not set ", stringify!(#field_name)))?
            }
        };

        quote! {
            #field_name : self.#field_name.#handler
        }
    })
}

fn get_name_and_type(f: &Field) -> (&Ident, &Type) {
    (f.ident.as_ref().unwrap(), &f.ty)
}

fn option_inner_type(ty: &Type) -> Option<&Type> {
    generic_inner_type(ty, "Option")
}

use syn::{GenericArgument, PathArguments};

fn generic_inner_type<'a>(ty: &'a Type, type_name: &str) -> Option<&'a Type> {
    let path = match ty {
        Type::Path(p) => &p.path,
        _ => return None,
    };

    if path.leading_colon.is_some() {
        return None;
    }

    if path.segments.len() != 1 || path.segments[0].ident != type_name {
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
        GenericArgument::Type(t) => Some(t),
        _ => None,
    }
}
