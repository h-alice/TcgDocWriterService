#---------------------------------------------------------------------
# Stage 1: Build Stage (using a standard Haskell image)
#---------------------------------------------------------------------
# Use an official Haskell image with a specific GHC version for reproducibility
FROM haskell:9.6-slim AS builder
# Or use haskell:latest if you prefer

# Set the working directory inside the container
WORKDIR /app

# Copy stack.yaml and package.yaml or *.cabal and install dependencies
COPY stack.yaml stack.yaml.lock ./
COPY package.yaml ./
RUN stack setup && stack build --only-dependencies

# Copy all source files
COPY . .

# Build the application
RUN stack build --copy-bins

#---------------------------------------------------------------------
# Stage 2: Runtime Stage (using Alpine)
#---------------------------------------------------------------------
# Use a specific Alpine version for stability
FROM alpine:3.21 AS final

# Install necessary system libs (if needed)
RUN apk add --no-cache \
    gmp     \
    libffi  \
    zlib    \
    libc6-compat \
    libstdc++ \
    libgcc 

# --- Security Best Practice: Run as non-root user ---
# Create a group and user
RUN addgroup -S appgroup && adduser -S appuser -G appgroup
# Set the user for subsequent commands
USER appuser

WORKDIR /app


# Copy the built binary from the builder stage
COPY --from=builder /app/bin/tcgdoc-serv-exe .

# Set port and command to run the app
EXPOSE 8080
CMD ["./tcgdoc-serv-exe"]
