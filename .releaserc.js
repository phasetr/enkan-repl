module.exports = {
  branches: ["main"],
  plugins: [
    // Analyze commits to determine version bump
    "@semantic-release/commit-analyzer",
    
    // Generate release notes from commits
    "@semantic-release/release-notes-generator",
    
    // Generate and update CHANGELOG.md
    [
      "@semantic-release/changelog",
      {
        changelogFile: "CHANGELOG.md",
      },
    ],
    
    // Update version in claudemacs-repl.el and regenerate default.org
    [
      "@semantic-release/exec",
      {
        prepareCmd: [
          // Update version in .el file
          "emacs --batch --load scripts/bump-version.el -- ${nextRelease.version}",
          // Regenerate default.org with updated version
          "emacs --batch --load scripts/generate-docs.el --eval '(generate-default-template)'"
        ].join(" && "),
      },
    ],
    
    // Commit the changes
    [
      "@semantic-release/git",
      {
        assets: [
          "CHANGELOG.md", 
          "package.json", 
          "claudemacs-repl.el",
          "default.org"
        ],
        message:
          "chore(release): ${nextRelease.version} [skip ci]\n\n${nextRelease.notes}",
      },
    ],
    
    // Create GitHub release
    "@semantic-release/github",
  ],
};