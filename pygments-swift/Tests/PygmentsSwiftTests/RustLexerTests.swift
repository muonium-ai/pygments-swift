import XCTest
@testable import PygmentsSwift

final class RustLexerTests: XCTestCase {
    func testRustLexingBasics() {
        let lexer = RustLexer()
        let input = """
        //! crate docs
        #[derive(Debug)]
        pub struct Foo { x: i32 }

        // comment
        fn main() {
            let s = r#"hi"#;
            println!("{}", s);
            let n: u32 = 42;
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(200).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "struct" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name && $0.value == "println" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
