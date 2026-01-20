import XCTest
@testable import PygmentsSwift

final class EiffelLexerTests: XCTestCase {
    func testEiffelBasics() {
        let lexer = EiffelLexer()
        let input = """
        -- comment
        class FIBONACCI
        feature
            fib (n: INTEGER): INTEGER
            do
                if n < 2 then
                    Result := n
                end
            end
        end
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "class" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
