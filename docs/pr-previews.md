# Pull request previews

Pull requests from branches in this repository build and test the game, then publish it at:

`https://rcmartins-apps.github.io/time-loop/pr-preview/pr-<number>/`

Open the PR's deployment link or the **Preview link** section in its **PR Test** workflow run. Allow GitHub Pages a little time to publish after the deployment finishes. New pushes update the same URL; closing or merging the PR removes its preview files.

Previews use separate browser saves for each PR. The main site's existing save stays unchanged. All sections use relative asset paths, so the game also runs under the preview subdirectory.

Main-branch deployments preserve the `pr-preview/` directory. Publishing and preview cleanup share a concurrency group to avoid simultaneous pushes to `gh-pages`.

Fork pull requests still run build/tests, but do not publish previews because their GitHub token has no write access. Preview builds run with read-only permissions; a separate deployment job downloads the static artifact and publishes it without executing PR code with write permissions.
