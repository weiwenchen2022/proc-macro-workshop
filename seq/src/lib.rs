use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::{format_ident, quote};

use syn::buffer::{Cursor, TokenBuffer};
use syn::parse::{Parse, ParseStream};
use syn::{braced, parse_macro_input, Ident, LitInt, Token};

struct Seq {
    variable: Ident,
    start: usize,
    end: usize,
    inclusive: bool,
    body: proc_macro2::TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let variable: Ident = input.parse()?;
        let _: Token![in] = input.parse()?;
        let start: LitInt = input.parse()?;
        let start = start.base10_parse()?;
        let _: Token![..] = input.parse()?;
        let inclusive = if input.peek(Token![=]) {
            let _: Token![=] = input.parse().unwrap();
            true
        } else {
            false
        };

        let end: LitInt = input.parse()?;
        let end = end.base10_parse()?;

        let body;
        braced!(body in input);
        let body: proc_macro2::TokenStream = body.parse()?;

        Ok(Self {
            variable,
            start,
            end,
            inclusive,
            body,
        })
    }
}

impl Seq {
    fn find_block_to_expand_and_do_expand(
        &self,
        mut cursor: Cursor,
    ) -> (proc_macro2::TokenStream, bool) {
        let mut found = false;
        let mut ret = proc_macro2::TokenStream::new();

        while !cursor.eof() {
            if let Some((punct_prefix, cursor_1)) = cursor.punct() {
                if '#' == punct_prefix.as_char() {
                    if let Some((group_cur, _, cursor_2)) =
                        cursor_1.group(proc_macro2::Delimiter::Parenthesis)
                    {
                        if let Some((punct_suffix, cursor_3)) = cursor_2.punct() {
                            if '*' == punct_suffix.as_char() {
                                if self.inclusive {
                                    (self.start..=self.end).for_each(|i| {
                                        let t = self.expand(group_cur.token_stream(), i);
                                        ret.extend(t);
                                    });
                                } else {
                                    (self.start..self.end).for_each(|i| {
                                        let t = self.expand(group_cur.token_stream(), i);
                                        ret.extend(t);
                                    });
                                }

                                cursor = cursor_3;
                                found = true;
                                continue;
                            }
                        }
                    }
                }
            }

            if let Some((group_cur, _, next_cur)) = cursor.group(proc_macro2::Delimiter::Brace) {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote! ({#t}));
                cursor = next_cur;
            } else if let Some((group_cur, _, next_cur)) =
                cursor.group(proc_macro2::Delimiter::Bracket)
            {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote! {[#t]});
                cursor = next_cur;
            } else if let Some((group_cur, _, next_cur)) =
                cursor.group(proc_macro2::Delimiter::Parenthesis)
            {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote! {(#t)});
                cursor = next_cur;
            } else if let Some((punct, next_cur)) = cursor.punct() {
                ret.extend(quote! {#punct});
                cursor = next_cur;
            } else if let Some((ident, next_cur)) = cursor.ident() {
                ret.extend(quote! {#ident});
                cursor = next_cur;
            } else if let Some((lietral, next_cur)) = cursor.literal() {
                ret.extend(quote! {#lietral});
                cursor = next_cur;
            } else if let Some((lifetime, next_cur)) = cursor.lifetime() {
                ret.extend(quote! {#lifetime});
                cursor = next_cur;
            }
        }

        (ret, found)
    }

    fn expand(&self, body: proc_macro2::TokenStream, n: usize) -> proc_macro2::TokenStream {
        let body = body.into_iter().collect::<Vec<_>>();

        let mut idx = 0;
        let mut ret = proc_macro2::TokenStream::new();

        while idx < body.len() {
            let tt = &body[idx];

            match tt {
                TokenTree::Group(g) => {
                    let new_group = self.expand(g.stream(), n);
                    let mut new_group = proc_macro2::Group::new(g.delimiter(), new_group);
                    new_group.set_span(g.span());
                    ret.extend(quote! {
                        #new_group
                    });
                }

                TokenTree::Ident(prefix) => {
                    if idx + 2 < body.len() {
                        if let TokenTree::Punct(p) = &body[idx + 1] {
                            if p.as_char() == '~' {
                                if let TokenTree::Ident(i) = &body[idx + 2] {
                                    if &self.variable == i
                                        && prefix.span().end() == p.span().start()
                                        && p.span().end() == i.span().start()
                                    {
                                        let new_ident = format_ident!("{}{}", prefix, n);
                                        ret.extend(quote! {#new_ident});
                                        idx += 3;
                                        continue;
                                    }
                                }
                            }
                        }
                    }

                    if &self.variable == prefix {
                        let new_ident = proc_macro2::Literal::usize_unsuffixed(n);
                        ret.extend(quote! {
                            #new_ident
                        });
                    } else {
                        ret.extend(quote! {
                            #tt
                        });
                    }
                }

                TokenTree::Punct(_) | TokenTree::Literal(_) => ret.extend(quote! {
                    #tt
                }),
            }

            idx += 1;
        }

        ret
    }
}

#[proc_macro]
pub fn seq(item: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(item as Seq);

    let buffer = TokenBuffer::new2(seq.body.clone());
    let (ret, expanded) = seq.find_block_to_expand_and_do_expand(buffer.begin());
    if expanded {
        return ret.into();
    }

    let body = if seq.inclusive {
        (seq.start..=seq.end)
            .map(|i| seq.expand(seq.body.clone(), i))
            .collect::<Vec<_>>()
    } else {
        (seq.start..seq.end)
            .map(|i| seq.expand(seq.body.clone(), i))
            .collect::<Vec<_>>()
    };

    quote! {
        #(#body)*
    }
    .into()
}
