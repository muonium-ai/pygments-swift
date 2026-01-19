import XCTest
@testable import PygmentsSwift

final class ScalaLexerTests: XCTestCase {
    func testScalaLexingBasics() {
        let lexer = ScalaLexer()
        let input = #"""
        package demo

        // comment
        /* block
           comment */

        @main
        object App {
          def main(args: Array[String]): Unit = {
            val n = 42
            val s = s"hi $n"
            val t = """triple"""
            println(s + t)
          }
        }
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "def" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "val" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "@main" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
