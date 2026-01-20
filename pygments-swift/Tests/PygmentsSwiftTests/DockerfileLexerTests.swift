import XCTest
@testable import PygmentsSwift

final class DockerfileLexerTests: XCTestCase {
    func testDockerfileBasics() {
        let lexer = DockerfileLexer()
        let input = """
        # comment
        FROM swift:5.9
        ARG CONFIG=release
        ENV CONFIG=${CONFIG}
        RUN echo \"hi\"
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.uppercased() == "FROM" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value.contains("CONFIG") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
