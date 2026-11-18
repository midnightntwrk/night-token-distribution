# Contributing

## Developer Certificate of Origin (DCO)
All contributions must include a sign-off in every commit message, certifying that you have the right to submit the code under the project license. This is done by adding a `Signed-off-by` trailer using `git commit -s`:

```
git commit -s -m "feat: your commit message"
```

This produces a commit message like:

```
feat: your commit message

Signed-off-by: Your Name <your@email.com>
```

By signing off, you agree to the [Developer Certificate of Origin (version 1.1)](https://developercertificate.org/).

If you have forgotten to sign off past commits in a PR, you can amend them:

```bash
# Amend the last commit
git commit --amend -s --no-edit

# Or rebase to sign off multiple commits (replace N with the number of commits)
git rebase --signoff HEAD~N
```

The DCO GitHub App checks pull requests for sign-offs. The required `DCO` check is configured separately through repository protections managed in `midnight-iac`.

### Automating sign-off

To avoid having to remember `-s` on every commit, install a `prepare-commit-msg` hook in your clone of this repo that appends the sign-off automatically:

```bash
cat > .git/hooks/prepare-commit-msg <<'EOF'
#!/bin/sh
NAME=$(git config user.name)
EMAIL=$(git config user.email)
grep -qs "^Signed-off-by: " "$1" || printf "\nSigned-off-by: %s <%s>\n" "$NAME" "$EMAIL" >> "$1"
EOF
chmod +x .git/hooks/prepare-commit-msg
```

After installing the hook, every `git commit` in this repo will include a `Signed-off-by` trailer automatically. Make sure your `user.name` and `user.email` are set correctly, since the hook certifies the DCO on your behalf for every commit.
