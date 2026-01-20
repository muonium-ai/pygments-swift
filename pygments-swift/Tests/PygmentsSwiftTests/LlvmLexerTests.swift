import XCTest
@testable import PygmentsSwift

final class LlvmLexerTests: XCTestCase {
    func testLlvmBasics() {
        let lexer = LlvmLexer()
        let input = """
        ; comment
        define i32 @fib(i32 %n) {
        entry:
          %cmp = icmp slt i32 %n, 2
          br i1 %cmp, label %ret, label %rec
        ret:
          ret i32 %n
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "define" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value.hasPrefix("%") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
