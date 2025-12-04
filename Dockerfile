FROM eclipse-temurin:21-jre-alpine

# Create app directory
WORKDIR /opt/app

# Copy fat JAR into the image
COPY target/scala-2.13/qure-assembly-0.1.20-SNAPSHOT.jar app.jar

# Tweak JVM options for a small Fargate task
ENV JAVA_OPTS="-Xms256m -Xmx256m"

# Run your app
ENTRYPOINT ["java -jar app.jar"]
