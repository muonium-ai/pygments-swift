import XCTest
@testable import PygmentsSwift

final class LatexLexerTests: XCTestCase {
    func testLatexBasics() {
        let lexer = LatexLexer()
        let input = """
        % comment
        \\section{Fibonacci}
        The recurrence is $F_n = F_{n-1} + F_{n-2}$.
        \\begin{equation}
        F_{10} = 55
        \\end{equation}
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "\\section" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("F_n") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "55" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
