import XCTest
@testable import PygmentsSwift

final class ProtobufLexerTests: XCTestCase {
    func testProtoBasics() {
        let lexer = ProtobufLexer()
        let input = """
        // comment
        syntax = "proto3";
        package demo;

        message FibRequest { int32 n = 1; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "message" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("proto3") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
