import XCTest
@testable import PygmentsSwift

final class RubyLexerTests: XCTestCase {
    func testRubyLexingBasics() {
        let lexer = RubyLexer()
        let input = """
        # comment
        class Foo
          def bar(x = 42)
            s = "hi #{x}"
            sym = :ok
            return s
          end
        end
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(220).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "class" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "def" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
