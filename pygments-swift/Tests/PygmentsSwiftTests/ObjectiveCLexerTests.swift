import XCTest
@testable import PygmentsSwift

final class ObjectiveCLexerTests: XCTestCase {
    func testObjectiveCBasics() {
        let lexer = ObjectiveCLexer()
        let input = """
        // comment
        @interface Fib : NSObject
        @end

        @implementation Fib
        + (int)fib:(int)n { return n < 2 ? n : 0; }
        @end

        int main(void) { @autoreleasepool { NSLog(@"%d", 10); } return 0; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "@interface" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("@\"%d\"") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
