import XCTest
@testable import PygmentsSwift

final class GoLexerTests: XCTestCase {
    func testGoLexingBasics() {
        let lexer = GoLexer()
        let input = """
        package main

        import "fmt"

        // comment
        func main() {
            s := `raw\n`
            fmt.Println("hi", s, 42)
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(160).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "package" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "func" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
