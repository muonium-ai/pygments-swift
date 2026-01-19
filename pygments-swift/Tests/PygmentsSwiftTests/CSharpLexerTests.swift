import XCTest
@testable import PygmentsSwift

final class CSharpLexerTests: XCTestCase {
    func testCSharpLexingBasics() {
        let lexer = CSharpLexer()
        let input = """
        using System;

        [Obsolete("old")]
        public class Foo {
            // comment
            public const int N = 42;
            public string Path = @"C:\\temp\\file.txt";
            public int Add(int a, int b) { return a + b; }
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(160).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "class" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword.child("Type") && $0.value == "string" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "Obsolete" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
