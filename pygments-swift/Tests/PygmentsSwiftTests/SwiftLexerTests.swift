import XCTest
@testable import PygmentsSwift

final class SwiftLexerTests: XCTestCase {
    func testSwiftLexingBasics() {
        let lexer = SwiftLexer()
        let input = """
        import Foundation

        struct Foo {
            let x: Int = 42
            func bar() -> String { return "hi" }
        }

        // Nested comment
        /* outer /* inner */ done */

        // interpolation
        let s = "value: \\(x)"
        """

        let tokens = lexer.getTokens(input)

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }))

        // Ensure we don't emit Error for basic valid Swift.
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }))
    }

    func testSwiftLexingUnicodeIdentifiers() {
        let lexer = SwiftLexer()
        let input = """
        struct 变量 {
            let café = 1
            func naïve(输入: Int) -> Int { return 输入 }
        }
        """

        let tokens = lexer.getTokens(input)

        let nameLike = tokens
            .filter { $0.type.isSubtype(of: .name) }
            .map { "\($0.type)=\($0.value)" }
            .joined(separator: ", ")

        XCTAssertTrue(
            tokens.contains(where: { $0.type == .name.child("Class") && $0.value == "变量" }),
            "Missing Name.Class '变量'. Got: \(nameLike)"
        )
        XCTAssertTrue(
            tokens.contains(where: { $0.type == .name.child("Variable") && $0.value == "café" }),
            "Missing Name.Variable 'café'. Got: \(nameLike)"
        )
        XCTAssertTrue(
            tokens.contains(where: { $0.type == .name.child("Function") && $0.value == "naïve" }),
            "Missing Name.Function 'naïve'. Got: \(nameLike)"
        )
        XCTAssertTrue(
            tokens.contains(where: { $0.type == .name && $0.value == "输入" }),
            "Missing Name '输入'. Got: \(nameLike)"
        )

        // Unicode identifiers should not cause fallback Error tokens.
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }))
    }
}
