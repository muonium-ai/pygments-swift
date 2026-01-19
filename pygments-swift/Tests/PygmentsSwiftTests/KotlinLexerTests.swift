import XCTest
@testable import PygmentsSwift

final class KotlinLexerTests: XCTestCase {
    func testKotlinLexingBasics() {
        let lexer = KotlinLexer()
        let input = """
        package demo

        // comment
        @JvmStatic
        fun main() {
            val s = "hi\\n"
            val t = \"\"\"triple\"\"\"
            val n = 42
            println(s + t + n)
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(200).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "@JvmStatic" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "fun" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
