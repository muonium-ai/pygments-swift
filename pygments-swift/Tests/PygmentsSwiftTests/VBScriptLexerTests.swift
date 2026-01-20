import XCTest
@testable import PygmentsSwift

final class VBScriptLexerTests: XCTestCase {
    func testVBScriptBasics() {
        let lexer = VBScriptLexer()
        let input = """
        ' comment
        Dim n
        n = 10
        Function fib(k)
          If k < 2 Then
            fib = k
          End If
        End Function
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "function" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "10" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
