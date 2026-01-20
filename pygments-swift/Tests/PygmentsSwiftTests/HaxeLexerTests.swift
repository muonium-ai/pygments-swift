import XCTest
@testable import PygmentsSwift

final class HaxeLexerTests: XCTestCase {
    func testHaxeBasics() {
        let lexer = HaxeLexer()
        let input = """
        // comment
        @:keep
        class Main {
          static function fib(n:Int):Int {
            if (n < 2) return n;
            return fib(n-1) + fib(n-2);
          }
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "class" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
