module.exports = {
	branches: ["main"],
	plugins: [
		// Analyze commits to determine version bump
		[
			"@semantic-release/commit-analyzer",
			{
				preset: "conventionalcommits",
				releaseRules: [
					{ type: "feat", release: "minor" },
					{ type: "fix", release: "patch" },
					{ type: "perf", release: "patch" },
					{ breaking: true, release: "minor" }, // Breaking changes in 0.x should be minor
				],
				presetConfig: {
					types: [
						{ type: "feat", section: "Features" },
						{ type: "fix", section: "Bug Fixes" },
						{ type: "chore", hidden: true },
						{ type: "docs", hidden: true },
						{ type: "style", hidden: true },
						{ type: "refactor", hidden: true },
						{ type: "perf", section: "Performance Improvements" },
						{ type: "test", hidden: true },
					],
				},
			},
		],
		// Generate release notes from commits
		"@semantic-release/release-notes-generator",
		// Generate and update CHANGELOG.md
		[
			"@semantic-release/changelog",
			{
				changelogFile: "CHANGELOG.md",
			},
		],
		// Update version in enkan-repl.el and regenerate documentation
		[
			"@semantic-release/exec",
			{
				prepareCmd: [
					// Update version in .el file
					"emacs --batch --load scripts/bump-version.el -- ${nextRelease.version}",
					// Regenerate documentation files with updated functions
					"emacs --batch --load scripts/generate-docs.el --eval '(generate-all-docs)'",
					// Regenerate precompiled constants for cheat-sheet performance
					"emacs --batch --load scripts/generate-constants.el --eval '(generate-cheat-sheet-constants)'",
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
					"enkan-repl.el",
					"README.org",
					"enkan-repl-constants.el",
				],
				message:
					"chore(release): ${nextRelease.version} [skip ci]\n\n${nextRelease.notes}",
			},
		],
		// Create GitHub release
		"@semantic-release/github",
	],
};
