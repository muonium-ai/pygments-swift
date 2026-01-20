import XCTest
@testable import PygmentsSwift

final class HlslLexerTests: XCTestCase {
    func testHlslBasics() {
        let lexer = HlslLexer()
        let input = """
        #define ONE 1
        // comment
        float fib(float n) { if (n < 2) return n; return fib(n-1) + fib(n-2); }
        float4 main(float4 pos : POSITION) : SV_Position { return pos; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "return" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) && $0.value.contains("#") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
