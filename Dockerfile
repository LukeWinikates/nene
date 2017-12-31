FROM clojure
COPY . /usr/src/app
WORKDIR /usr/src/app
CMD ["lein", "ring", "server-headless"]

EXPOSE 3000