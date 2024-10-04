use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Item, ItemFn};

use proc_macro2::Span;

#[proc_macro_attribute]
pub fn sorted(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as Item);

    let ast = match &ast {
        Item::Enum(e) => e,
        _ => {
            return syn::Error::new(Span::call_site(), "expected enum or match expression")
                .to_compile_error()
                .into()
        }
    };

    let mut variants_sorted = ast.variants.iter().collect::<Vec<_>>();
    variants_sorted.sort_by_key(|v| &v.ident);

    let compile_error = match ast.variants.iter().zip(variants_sorted.iter()).try_fold(
        (),
        |_, (variant, &sorted_variant)| {
            // let ident = &variant.ident;

            if variant.ident != sorted_variant.ident {
                return Err(syn::Error::new(
                    sorted_variant.span(),
                    format!(
                        "{} should sort before {}",
                        &sorted_variant.ident, &variant.ident
                    ),
                ));
            }

            Ok(())
        },
    ) {
        Ok(_) => quote! {},
        Err(err) => err.to_compile_error(),
    };

    quote! {
        #compile_error

        #ast
    }
    .into()
}

use syn::visit_mut::{self, VisitMut};
use syn::{ExprMatch, Ident, Pat, PatTupleStruct};
use syn::{ExprPath, PatStruct};

#[derive(Default)]
struct ExprMatchVisitor {
    error: Option<syn::Error>,
}

impl VisitMut for ExprMatchVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        if let Some(attr) = i
            .attrs
            .iter()
            .position(|attr| attr.path().is_ident("sorted"))
        {
            i.attrs.remove(attr);

            let pats = i
                .arms
                .iter()
                .map(|arm| {
                    let pat = &arm.pat;
                    let span: &dyn ToTokens;

                    let paths = match pat {
                        Pat::Ident(p) => {
                            span = &p.ident;
                            vec![p.ident.clone()]
                        }

                        Pat::Path(ExprPath { path, .. })
                        | Pat::TupleStruct(PatTupleStruct { path, .. })
                        | Pat::Struct(PatStruct { path, .. }) => {
                            span = path;

                            path.segments
                                .iter()
                                .map(|s| &s.ident)
                                .cloned()
                                .collect::<Vec<_>>()
                        }

                        Pat::Wild(p) => {
                            span = p;
                            vec![Ident::new(
                                &p.underscore_token.to_token_stream().to_string(),
                                p.span(),
                            )]
                        }

                        pat => {
                            return Err(syn::Error::new_spanned(pat, "unsupported by #[sorted]"));
                        }
                    };

                    Ok((paths, span))
                })
                .collect::<syn::Result<Vec<_>>>();

            match pats {
                Ok(pats) => {
                    let mut pats_sorted = pats.clone();
                    pats_sorted.sort_by(|(paths1, _), (paths2, _)| paths1.cmp(paths2));

                    let _ = pats
                        .iter()
                        .zip(pats_sorted.iter())
                        .try_for_each(|(p1, p2)| {
                            if p1.0 != p2.0 {
                                if self.error.is_none() {
                                    self.error = Some(syn::Error::new_spanned(
                                        p2.1,
                                        format!(
                                            "{} should sort before {}",
                                            p2.0.iter()
                                                .map(|p| p.to_string())
                                                .collect::<Vec<_>>()
                                                .join("::"),
                                            p1.0.iter()
                                                .map(|p| p.to_string())
                                                .collect::<Vec<_>>()
                                                .join("::"),
                                        ),
                                    ));
                                }

                                return Err(());
                            }

                            Ok(())
                        });
                }
                Err(err) => {
                    if self.error.is_none() {
                        self.error = Some(err);
                    }
                }
            };
        }

        visit_mut::visit_expr_match_mut(self, i);
    }
}

#[proc_macro_attribute]
pub fn check(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(item as ItemFn);

    let mut visitor = ExprMatchVisitor::default();
    visitor.visit_item_fn_mut(&mut ast);

    let compile_error = visitor
        .error
        .map(|err| err.to_compile_error())
        .unwrap_or_else(|| quote! {});

    quote! {
        #compile_error

        #ast
    }
    .into()
}
