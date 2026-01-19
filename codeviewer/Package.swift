// swift-tools-version: 5.9

import PackageDescription

let package = Package(
    name: "codeviewer",
    platforms: [
        .macOS(.v13)
    ],
    dependencies: [
        .package(path: "../pygments-swift")
    ],
    targets: [
        .executableTarget(
            name: "codeviewer",
            dependencies: [
                .product(name: "PygmentsSwift", package: "pygments-swift")
            ]
        )
    ]
)
