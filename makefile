deploy:
	heroku apps:create ent-dev-frontend --region eu && git push heroku master
	heroku config:set HOST=ent-dev-frontend
	heroku config:set PORT=5000
	heroku config:set NODE_HOST=ent-dev-backend
	heroku logs --tail
undeploy:
	git remote remove heroku
	heroku apps:delete ent-dev-frontend --confirm ent-dev-frontend
redeploy: undeploy deploy