# Build stage
FROM node:20-alpine AS build
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci

# Copy source code
COPY . .

# Build the application
RUN npm run build

# Production stage
FROM node:20-alpine
WORKDIR /app

# Install serve to run the production build
RUN npm install -g serve

# Copy built assets from build stage
COPY --from=build /app/dist ./dist

# Expose port (Railway will set PORT env var)
EXPOSE 5173

# Start the application (use PORT env var or default to 5173)
CMD sh -c "serve -s dist -l ${PORT:-5173}"
