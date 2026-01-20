import XCTest
@testable import PygmentsSwift

final class SolidityLexerTests: XCTestCase {
    func testSolidityBasics() {
        let lexer = SolidityLexer()
        let input = """
        // comment
        pragma solidity ^0.8.20;

        contract Fib {
            function fib(uint n) public pure returns (uint) {
                if (n < 2) return n;
                return fib(n - 1) + fib(n - 2);
            }
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "contract" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "2" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
