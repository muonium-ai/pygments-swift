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
}
