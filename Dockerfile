# Build stage 0
FROM erlang:alpine

# Install Rebar3
RUN mkdir -p /buildroot/rebar3/bin
ADD https://s3.amazonaws.com/rebar3/rebar3 /buildroot/rebar3/bin/rebar3
RUN chmod a+x /buildroot/rebar3/bin/rebar3

# Install git
RUN apk add --no-cache git build-base

# Setup Environment
ENV PATH=/buildroot/rebar3/bin:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang application
COPY . bloberl

# And build the release
WORKDIR bloberl
RUN rebar3 as prod release

# Build stage 1
FROM alpine:3.9

RUN addgroup -S -g 10101 bloberl && adduser -S -D -u 10101 -g bloberl bloberl

# Install some libs
RUN apk add --no-cache openssl ncurses-libs libstdc++ && \
    rm -rf /tmp/* /var/tmp/*

# Install the released application
COPY --from=0 /buildroot/bloberl/_build/prod/rel/bloberl /bloberl

# Expose relevant ports
EXPOSE 31090

USER 10101

CMD ["/bloberl/bin/bloberl", "foreground"]
