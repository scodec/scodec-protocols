package scodec.protocols

import java.io.{ File, FileInputStream }

object ExampleData {
  def mpegPcapChannel = {
    print("Enter path to a pcap mpeg file: ")
    val line = readLine()
    val file = new File(line)
    new FileInputStream(file).getChannel
  }
}
