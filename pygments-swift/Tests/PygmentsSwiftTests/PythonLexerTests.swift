import XCTest
@testable import PygmentsSwift

final class PythonLexerTests: XCTestCase {
    func testPythonLexingBasics() {
        let lexer = PythonLexer()
        let input = """
        # comment
        @decorator
        class Foo:
            def bar(self, x=42):
                s = f\"hi {x}\\n\"
                return s
        """

        let tokens = lexer.getTokens(input)

        let summary = tokens.prefix(80).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }))
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "@decorator" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Class") && $0.value == "Foo" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Function") && $0.value == "bar" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }))
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }))

        // For this pragmatic lexer, we still expect not to emit Error for basic code.
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }))
    }
}
