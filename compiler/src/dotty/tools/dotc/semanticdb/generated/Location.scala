// Generated by https://github.com/tanishiking/semanticdb-for-scala3
// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO3

package dotty.tools.dotc.semanticdb
import dotty.tools.dotc.semanticdb.internal.*
import scala.annotation.internal.sharable

@SerialVersionUID(0L)
final case class Location(
    uri: _root_.scala.Predef.String = "",
    range: _root_.scala.Option[dotty.tools.dotc.semanticdb.Range] =
      _root_.scala.None
) extends SemanticdbGeneratedMessage
    derives CanEqual:
  @transient @sharable
  private[this] var __serializedSizeMemoized: _root_.scala.Int = 0
  private[this] def __computeSerializedSize(): _root_.scala.Int =
    var __size = 0

    {
      val __value = uri
      if !__value.isEmpty then
        __size += SemanticdbOutputStream.computeStringSize(1, __value)
    };
    if range.isDefined then
      val __value = range.get
      __size += 1 + SemanticdbOutputStream.computeUInt32SizeNoTag(
        __value.serializedSize
      ) + __value.serializedSize;
    __size
  override def serializedSize: _root_.scala.Int =
    var __size = __serializedSizeMemoized
    if __size == 0 then
      __size = __computeSerializedSize() + 1
      __serializedSizeMemoized = __size
    __size - 1

  def writeTo(`_output__`: SemanticdbOutputStream): _root_.scala.Unit =
    {
      val __v = uri
      if !__v.isEmpty then _output__.writeString(1, __v)
    };
    range.foreach { __v =>
      val __m = __v
      _output__.writeTag(2, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
  def withUri(__v: _root_.scala.Predef.String): Location = copy(uri = __v)
  def getRange: dotty.tools.dotc.semanticdb.Range =
    range.getOrElse(dotty.tools.dotc.semanticdb.Range.defaultInstance)
  def clearRange: Location = copy(range = _root_.scala.None)
  def withRange(__v: dotty.tools.dotc.semanticdb.Range): Location =
    copy(range = Option(__v))

  // @@protoc_insertion_point(GeneratedMessage[dotty.tools.dotc.semanticdb.Location])
end Location

object Location
    extends SemanticdbGeneratedMessageCompanion[
      dotty.tools.dotc.semanticdb.Location
    ]:
  implicit def messageCompanion: SemanticdbGeneratedMessageCompanion[
    dotty.tools.dotc.semanticdb.Location
  ] = this
  def parseFrom(
      `_input__`: SemanticdbInputStream
  ): dotty.tools.dotc.semanticdb.Location =
    var __uri: _root_.scala.Predef.String = ""
    var __range: _root_.scala.Option[dotty.tools.dotc.semanticdb.Range] =
      _root_.scala.None
    var _done__ = false
    while !_done__ do
      val _tag__ = _input__.readTag()
      _tag__ match
        case 0 => _done__ = true
        case 10 =>
          __uri = _input__.readStringRequireUtf8()
        case 18 =>
          __range = Option(
            __range.fold(
              LiteParser
                .readMessage[dotty.tools.dotc.semanticdb.Range](_input__)
            )(LiteParser.readMessage(_input__, _))
          )
        case tag => _input__.skipField(tag)
    dotty.tools.dotc.semanticdb.Location(
      uri = __uri,
      range = __range
    )

  lazy val defaultInstance = dotty.tools.dotc.semanticdb.Location(
    uri = "",
    range = _root_.scala.None
  )
  final val URI_FIELD_NUMBER = 1
  final val RANGE_FIELD_NUMBER = 2
  def of(
      uri: _root_.scala.Predef.String,
      range: _root_.scala.Option[dotty.tools.dotc.semanticdb.Range]
  ): _root_.dotty.tools.dotc.semanticdb.Location =
    _root_.dotty.tools.dotc.semanticdb.Location(
      uri,
      range
    )
  // @@protoc_insertion_point(GeneratedMessageCompanion[dotty.tools.dotc.semanticdb.Location])
end Location
