FROM eclipse-temurin:17-jre-jammy

# Create app directory
WORKDIR /opt/app

# Initialize provider/env vars to empty strings
ENV IBM_INSTANCE_ID="" \
    IBM_API_KEY="" \
    AWS_ACCESS_ID="" \
    AWS_API_SECRET="" \
    AZURE_QUANTUM_API_KEY="" \
    AZURE_RESOURCE_GROUP="" \
    AZURE_SUB_ID="" \
    AZURE_WORKSPACE="" \
    SC_POSTGRES_PASSWORD="" \
    JAVA_OPTS="-Xms256m -Xmx256m"

# Copy required runtime assets
COPY mqt /opt/app/mqt

# Copy fat JAR into the image
COPY target/scala-2.13/qure-assembly-0.1.20-SNAPSHOT.jar /opt/app/app.jar

# Run your app
ENTRYPOINT ["sh", "-c", "exec java $JAVA_OPTS -jar /opt/app/app.jar"]