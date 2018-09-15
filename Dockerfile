FROM nginx:latest

COPY resources /usr/share/nginx/html/resources
COPY index.html /usr/share/nginx/html/
