# See .travis.yml how this docker image can be used.
FROM multiarch/ubuntu-debootstrap:powerpc-xenial

RUN uname -a
RUN apt-get update -qq && \
  apt-get install -yq \
    autoconf \
    automake \
    file \
    gcc \
    git \
    libtool \
    make \
    ruby-dev
RUN ruby --version

WORKDIR /ffi
COPY . .

RUN gem install bundler --no-doc && \
    bundle install

CMD bundle install && \
    bundle exec rake compile && \
    bundle exec rake test
