#
# ./publish "commit message here"
#

cd ./_site/
new_git=false
if [ ! -d ".git" ]; then
  cd ./_site/
  git init
  git remote add origin git@github.com:wetmore/wetmore.github.com.git
  new_git=true
fi
git add .
git commit -m "$1"
if [ $new_git ]; then
  git push origin master --force
else
  git push origin master
fi
