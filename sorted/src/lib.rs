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

            let _ = i.arms.iter().enumerate().try_fold(
                ((), None::<Vec<Ident>>),
                |(_, prev), (index, arm)| {
                    let pat = &arm.pat;
                    let span: &dyn ToTokens;
                    // eprintln!("pat: {:#?}", pat);

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

                        Pat::Wild(_) => {
                            if index != i.arms.len() - 1 {
                                if self.error.is_none() {
                                    self.error = Some(syn::Error::new_spanned(
                                        pat,
                                        "wildcard pattern should be present the last one",
                                    ));
                                }

                                return Err(());
                            }

                            return Ok(((), prev));
                        }

                        pat => {
                            if self.error.is_none() {
                                self.error =
                                    Some(syn::Error::new_spanned(pat, "unsupported by #[sorted]"));
                            }

                            return Err(());
                        }
                    };

                    if let Some(prev) = &prev {
                        #[allow(clippy::collapsible_if)]
                        if prev > &paths {
                            if self.error.is_none() {
                                self.error = Some(syn::Error::new_spanned(
                                    span,
                                    format!(
                                        "{} should sort before {}",
                                        paths
                                            .iter()
                                            .map(|p| p.to_string())
                                            .collect::<Vec<_>>()
                                            .join("::"),
                                        prev.iter()
                                            .map(|p| p.to_string())
                                            .collect::<Vec<_>>()
                                            .join("::"),
                                    ),
                                ));
                            }

                            return Err(());
                        }
                    }

                    Ok(((), Some(paths)))
                },
            );
        };

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
