 echo =========================================================
 echo COMMITING *.md FILES
 echo =========================================================

 find . -name "*md" | xargs git add ;  find . -name "*md" | xargs git commit -m "add md output" | git push git@github.com:BenjaminVanRyseghem/Spec_Documentation.git master
