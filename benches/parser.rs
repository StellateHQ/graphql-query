#[macro_use]
extern crate bencher;

use bencher::Bencher;

fn graphql_ast_parse_graphql_query_rs(bench: &mut Bencher) {
    use graphql_query::ast::*;
    bench.iter(|| {
        let ctx = ASTContext::new();
        Document::parse(&ctx, QUERY).ok();
    });
}

fn graphql_ast_parse_async_graphql_parser(bench: &mut Bencher) {
    use async_graphql_parser::parse_query;
    bench.iter(|| {
        parse_query::<&str>(QUERY).ok();
    });
}

fn graphql_ast_parse_graphql_parser(bench: &mut Bencher) {
    use graphql_parser::query::parse_query;
    bench.iter(|| {
        parse_query::<&str>(QUERY).ok();
    });
}

fn graphql_ast_parse_apollo_parser(bench: &mut Bencher) {
    use apollo_parser::Parser;
    bench.iter(|| {
        let parser = Parser::new(QUERY);
        let cst = parser.parse();
        cst.document();
    });
}

fn graphql_ast_print_graphql_query_rs(bench: &mut Bencher) {
    use graphql_query::ast::*;
    let ctx = ASTContext::new();
    let ast = Document::parse(&ctx, QUERY).unwrap();
    bench.iter(|| ast.print());
}

fn graphql_ast_print_gql_parser(bench: &mut Bencher) {
    use graphql_parser::query::parse_query;
    let ast = parse_query::<&str>(QUERY).ok().unwrap();
    bench.iter(|| ast.to_string());
}

fn graphql_ast_print_apollo_parser(bench: &mut Bencher) {
    use apollo_parser::cst::CstNode;
    use apollo_parser::Parser;
    let parser = Parser::new(QUERY);
    let cst = parser.parse();
    let doc = cst.document();
    bench.iter(|| doc.source_string());
}

fn graphql_ast_validate(bench: &mut Bencher) {
    use graphql_query::ast::*;
    use graphql_query::validate::*;
    let ctx = ASTContext::new();
    let ast = Document::parse(&ctx, QUERY).unwrap();
    bench.iter(|| ast.validate::<DefaultRules>(&ctx).unwrap());
}

fn graphql_ast_fold(bench: &mut Bencher) {
    use graphql_query::ast::*;
    use graphql_query::visit::*;

    #[derive(Default)]
    struct FoldNoop {}
    impl<'a> SimpleFolder<'a> for FoldNoop {
        fn named_type(&mut self, _name: NamedType<'a>) -> NamedType<'a> {
            NamedType { name: "oomph" }
        }
    }

    let ctx = ASTContext::new();
    let ast = Document::parse(&ctx, QUERY).unwrap();
    bench.iter(|| ast.fold(&ctx, &mut FoldNoop::default()).unwrap());
}

fn graphql_load_introspection(bench: &mut Bencher) {
    use graphql_query::ast::ASTContext;
    use graphql_query::schema::*;

    let ctx = ASTContext::new();

    bench.iter(|| {
        let introspection: IntrospectionQuery = serde_json::from_str(INTROSPECTION).unwrap();
        introspection.build_client_schema(&ctx);
    });
}

benchmark_group!(
    parse,
    graphql_ast_parse_graphql_query_rs,
    graphql_ast_parse_async_graphql_parser,
    graphql_ast_parse_graphql_parser,
    graphql_ast_parse_apollo_parser,
    graphql_ast_print_graphql_query_rs,
    graphql_ast_print_gql_parser,
    graphql_ast_print_apollo_parser,
    graphql_ast_validate,
    graphql_ast_fold,
    graphql_load_introspection
);

benchmark_main!(parse);

static QUERY: &str = include_str!("../fixture/kitchen_sink.graphql");
static INTROSPECTION: &str = include_str!("../fixture/introspection_query.json");
