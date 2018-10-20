# Use Node.js version 10
FROM mhart/alpine-node:10

# Set the working directory
WORKDIR /usr/src

# Copy package manager files to the working directory and run install
COPY package.json yarn.lock ./

RUN yarn add create-elm-app
RUN yarn install

# Copy all files to the working directory
COPY . .

# Run tests
# RUN CI=true yarn test

# Build the app and move the resulting build to the `/public` directory
#RUN yarn build
#RUN yarn build:ci
RUN yarn elm-app build
RUN mv ./build /public
