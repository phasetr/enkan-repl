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
		// Create GitHub release
		"@semantic-release/github",
	],
};
