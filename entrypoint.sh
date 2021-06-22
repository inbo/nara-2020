#!/bin/sh -l

echo '\nGetting the code...\n'
git clone --quiet https://$2@github.com/$1 /render
cd /render
git config advice.detachedHead false
if [ $3 -eq 0 ] ; then
  echo 'Checking out '$GITHUB_SHA'...\n'
  git checkout $GITHUB_SHA
else
  echo 'Checking out main...\n'
  git checkout main
fi
cp -r /render/. $GITHUB_WORKSPACE
cd $GITHUB_WORKSPACE
echo 'Rendering the Rmarkdown files...\n'
rm .Rprofile
Rscript "render.R"
if [ $? -ne 0 ]; then
  echo '\nRendering failed. Please check the error message above.\n';
  exit 1
fi
echo '\nAll Rmarkdown files rendered successfully\n'

if [ $3 -eq 0 ]; then
  echo 'Done.\n'
  exit 0
fi

echo 'Publishing the rendered files...\n'
git clone --branch=publish https://$2@github.com/$1 /publish
git config --global user.email "info@inbo.be"
git config --global user.name "INBO"
cd /publish
git rm -r .
cp -R $GITHUB_WORKSPACE/publish/. .
if ! git diff-index --quiet HEAD --; then
    git add --all
    git commit --amend --message="Te publiceren versie van de NARA-2020 achtergronddocumenten"
    git push -f
    echo '\nNew version published...'
else
  echo '\nNothing to update...'
fi
