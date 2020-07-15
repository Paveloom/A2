
     ## Это шаблон make-файла для публикации кода на GitHub.

     ## Репозиторий на GitHub: https://github.com/Paveloom/B1
     ## Документация: https://www.notion.so/paveloom/B1-fefcaf42ddf541d4b11cfcab63c2f018

     ## Версия релиза: 2.2.1
     ## Версия документации: 2.2.0

     ## Автор: Павел Соболев (http://paveloom.tk)

     ## Для корректного отображения содержимого
     ## символ табуляции должен визуально иметь
     ## ту же длину, что и пять пробелов.

     # Настройки make-файла

     ## Имя координатора
     make := make

     ## Указание оболочки
     SHELL := /bin/bash

     ## Указание make-файлу выполнять все правила в одном вызове оболочки
     .ONESHELL :

     ## Заглушка на вывод информации указанным правилам
     .SILENT :

     ## Правила-псевдоцели
     .PHONY : git, git-am, new, del, git-new, git-clean, archive

     ## Правило, выполняющееся при вызове координатора без аргументов
     ALL : git



     # Блок правил для разработки и публикации кода на GitHub

     ## Имя пользователя на GitHub
     username := Paveloom

     ## Сообщение стартового коммита
     start_message := "Стартовый коммит."

     ## Имя ветки изменений
     feature_branch := feature

     ## Правило для создания и публикации коммита
     git :
	      git add -A
	      git commit -e

	      # Проверка, был ли создан коммит
	      if [ $$? -eq 0 ]; then

	           git push --follow-tags

	      fi

     ## Правило для обновления последнего коммита до текущего состояния локального репозитория
     git-am :
	         git add -A
	         git commit --amend

	         # Проверка, был ли создан коммит
	         if [ $$? -eq 0 ]; then

	              git push --follow-tags --force-with-lease

	         fi

     ## Правило для создания ветки изменений
     new :
	      git checkout -q master
	      git checkout -b ${feature_branch}
	      git push -u origin ${feature_branch}

     ## Правило для удаления текущей ветки изменений локально
     del :
	      git checkout -q master
	      git branch -D ${feature_branch}

     ## Правило для подключения удалённого репозитория и
     ## загрузки в него стартового make-файла

     ifeq (git-new, $(firstword $(MAKECMDGOALS)))
          new_rep := $(wordlist 2, 2, $(MAKECMDGOALS))
          $(eval $(new_rep):;@#)
     endif

     git-new :
	          $(make) git-clean
	          git init
	          git remote add origin git@github.com:$(username)/$(new_rep).git
	          git add Makefile
	          git commit -m $(start_message)
	          git push -u origin master

     ## Правило для удаления репозитория в текущей директории
     git-clean :
	            rm -rf .git

     # Правило для создания архивов
     archive :
	          find Директория/ -path '*/.*' -prune -o -type f -print | zip Архивы/Директория.zip -FS -q -@
