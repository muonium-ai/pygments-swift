import XCTest
@testable import PygmentsSwift

final class FishLexerTests: XCTestCase {
    func testFishBasics() {
        let lexer = FishLexer()
        let input = """
        # comment
        function fib
            set -l n $argv[1]
            if test $n -lt 2
                echo $n
            end
        end
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value.contains("$n") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
