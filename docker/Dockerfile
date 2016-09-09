FROM phusion/baseimage

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update
RUN apt-get upgrade -y

# Install JRE
RUN apt-get install -y -q openjdk-7-jre-headless

# create user
RUN useradd --create-home --home /var/lib/webapp webapp

USER webapp
WORKDIR /var/lib/webapp
RUN mkdir bin
ENV PATH /var/lib/webapp/bin:$PATH
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

ADD ctgov-import-0.1.2-standalone.jar ctgov-import.jar

CMD java -jar ctgov-import.jar
