function kafkaExport() {
	cd ~/serhii.home/kafka-exports

	kafka-console-consumer.sh --bootstrap-server localhost:9092 \
                          --topic com.hitachirail.pass.notification \
                          --from-beginning \
                          --property print.timestamp=true \
                          --property print.key=true > $1 
}
