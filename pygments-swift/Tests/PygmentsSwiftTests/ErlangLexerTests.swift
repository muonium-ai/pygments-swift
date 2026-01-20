import XCTest
@testable import PygmentsSwift

final class ErlangLexerTests: XCTestCase {
    func testErlangBasics() {
        let lexer = ErlangLexer()
        let input = """
        % comment
        -module(fib).
        -export([fib/1]).

        fib(N) when N < 2 -> N;
        fib(N) -> fib(N - 1) + fib(N - 2).
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "module" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value == "N" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
