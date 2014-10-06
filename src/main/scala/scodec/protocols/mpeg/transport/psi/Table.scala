package scodec.protocols.mpeg
package transport
package psi

/**
 * Indicates the implementor can be treated as a message delivered in an MPEG transport stream.
 *
 * This library differentiates tables from sections. Sections are the actual messages delivered
 * in the transport stream whereas tables are the result of grouping multiple related sections
 * together in to a single logical message.
 */
trait Table {
  def tableId: Int
}
