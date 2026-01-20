import XCTest
@testable import PygmentsSwift

final class CrystalLexerTests: XCTestCase {
    func testCrystalBasics() {
        let lexer = CrystalLexer()
        let input = """
        # comment
        def fib(n : Int32) : Int32
          if n < 2
            n
          else
            fib(n-1) + fib(n-2)
          end
        end
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "def" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "end" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
