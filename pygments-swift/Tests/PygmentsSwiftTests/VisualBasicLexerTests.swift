import XCTest
@testable import PygmentsSwift

final class VisualBasicLexerTests: XCTestCase {
    func testVisualBasicBasics() {
        let lexer = VisualBasicLexer()
        let input = """
        ' comment
        Module Program
            Function Fib(n As Integer) As Integer
                If n < 2 Then
                    Return n
                End If
                Return Fib(n-1) + Fib(n-2)
            End Function
        End Module
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "module" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
