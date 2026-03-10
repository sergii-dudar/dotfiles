# info

also as example to tell jdtls to add generated source to class path (in case not picked by default):

```xml
    <plugin>
        <groupId>org.codehaus.mojo</groupId>
        <artifactId>build-helper-maven-plugin</artifactId>
        <executions>
            <execution>
                <id>add-graphql-source</id>
                <phase>generate-sources</phase>
                <goals>
                    <goal>add-source</goal>
                </goals>
                <configuration>
                    <sources>
                        <source>${project.build.directory}/generated-sources/graphql</source>
                    </sources>
                </configuration>
            </execution>
        </executions>
    </plugin>
```

but better to use [m2e-lifecycle-mappings](./m2e-lifecycle-mappings.xml) as it's working without chaning project pom.xml
