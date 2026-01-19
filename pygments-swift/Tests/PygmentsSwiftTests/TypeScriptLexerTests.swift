import XCTest
@testable import PygmentsSwift

final class TypeScriptLexerTests: XCTestCase {
    func testTypeScriptLexingBasics() {
        let lexer = TypeScriptLexer()
        let input = """
        // comment
        @sealed
        interface Foo<T> { x: number }
        type Bar = Foo<string>
        class Baz implements Foo<number> {
            readonly y: string = `hi ${1 + 2}`
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(120).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "@sealed" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Class") && $0.value == "Foo" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Class") && $0.value == "Baz" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword.child("Type") && $0.value == "number" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword.child("Type") && $0.value == "string" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)

        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
