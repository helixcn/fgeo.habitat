# MOST COMMON COMMANDS ====================================================

# Add all and commit ------------------------------------------------------

git add .
git commit -m ""

# Push --------------------------------------------------------------------

git pull
git push

# LESS COMMON COMMANDS ====================================================

# Cherry pick -------------------------------------------------------------

# Choose a commit from one branch and apply it onto another
# (https://goo.gl/TLoq22)

# Make sure you are on the branch you want to apply the commit to.
git checkout master

# Execute the following:
git cherry-pick <commit-hash>

# Forget ------------------------------------------------------------------

# Stop tracking a file that now is in .gitignore (https://goo.gl/54HCQn)

git rm --cached -r .
git add .
git update-index --assume-unchanged
git commit -a -m "forget cached"

# Move tag to differetn commit --------------------------------------------

# https://goo.gl/EokMoR

# Delete the tag on any remote before you push
git push origin :refs/tags/<tagname>

# Replace the tag to reference the most recent commit
git tag -fa <tagname>

# Push the tag to the remote origin
git push origin master --tags

# Push new repo -----------------------------------------------------------

git remote add origin https://github.com/forestgeo/repo}.git
git push -u origin master

# Search log --------------------------------------------------------------

# Search the commit log across all branches (https://goo.gl/fEVcJU)

git log --all --grep = ""




