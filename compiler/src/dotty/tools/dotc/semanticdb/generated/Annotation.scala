// Generated by https://github.com/tanishiking/semanticdb-for-scala3
// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO3

package dotty.tools.dotc.semanticdb
import dotty.tools.dotc.semanticdb.internal.*
import scala.annotation.internal.sharable

@SerialVersionUID(0L)
final case class Annotation(
    tpe: dotty.tools.dotc.semanticdb.Type =
      dotty.tools.dotc.semanticdb.Annotation._typemapper_tpe
        .toCustom(dotty.tools.dotc.semanticdb.TypeMessage.defaultInstance)
) extends SemanticdbGeneratedMessage
    derives CanEqual:
  @transient @sharable
  private[this] var __serializedSizeMemoized: _root_.scala.Int = 0
  private[this] def __computeSerializedSize(): _root_.scala.Int =
    var __size = 0

    {
      val __value =
        dotty.tools.dotc.semanticdb.Annotation._typemapper_tpe.toBase(tpe)
      if __value.serializedSize != 0 then
        __size += 1 + SemanticdbOutputStream.computeUInt32SizeNoTag(
          __value.serializedSize
        ) + __value.serializedSize
    };
    __size
  override def serializedSize: _root_.scala.Int =
    var __size = __serializedSizeMemoized
    if __size == 0 then
      __size = __computeSerializedSize() + 1
      __serializedSizeMemoized = __size
    __size - 1

  def writeTo(`_output__`: SemanticdbOutputStream): _root_.scala.Unit =
    val __v = dotty.tools.dotc.semanticdb.Annotation._typemapper_tpe.toBase(tpe)
    if __v.serializedSize != 0 then
      _output__.writeTag(1, 2)
      _output__.writeUInt32NoTag(__v.serializedSize)
      __v.writeTo(_output__)
  ;
  def withTpe(__v: dotty.tools.dotc.semanticdb.Type): Annotation =
    copy(tpe = __v)

  // @@protoc_insertion_point(GeneratedMessage[dotty.tools.dotc.semanticdb.Annotation])
end Annotation

object Annotation
    extends SemanticdbGeneratedMessageCompanion[
      dotty.tools.dotc.semanticdb.Annotation
    ]:
  implicit def messageCompanion: SemanticdbGeneratedMessageCompanion[
    dotty.tools.dotc.semanticdb.Annotation
  ] = this
  def parseFrom(
      `_input__`: SemanticdbInputStream
  ): dotty.tools.dotc.semanticdb.Annotation =
    var __tpe: _root_.scala.Option[dotty.tools.dotc.semanticdb.TypeMessage] =
      _root_.scala.None
    var _done__ = false
    while !_done__ do
      val _tag__ = _input__.readTag()
      _tag__ match
        case 0 => _done__ = true
        case 10 =>
          __tpe = _root_.scala.Some(
            __tpe.fold(
              LiteParser
                .readMessage[dotty.tools.dotc.semanticdb.TypeMessage](_input__)
            )(LiteParser.readMessage(_input__, _))
          )
        case tag => _input__.skipField(tag)
    dotty.tools.dotc.semanticdb.Annotation(
      tpe = dotty.tools.dotc.semanticdb.Annotation._typemapper_tpe.toCustom(
        __tpe.getOrElse(dotty.tools.dotc.semanticdb.TypeMessage.defaultInstance)
      )
    )

  lazy val defaultInstance = dotty.tools.dotc.semanticdb.Annotation(
    tpe = dotty.tools.dotc.semanticdb.Annotation._typemapper_tpe
      .toCustom(dotty.tools.dotc.semanticdb.TypeMessage.defaultInstance)
  )
  final val TPE_FIELD_NUMBER = 1
  @transient @sharable
  private[semanticdb] val _typemapper_tpe: SemanticdbTypeMapper[
    dotty.tools.dotc.semanticdb.TypeMessage,
    dotty.tools.dotc.semanticdb.Type
  ] = implicitly[SemanticdbTypeMapper[
    dotty.tools.dotc.semanticdb.TypeMessage,
    dotty.tools.dotc.semanticdb.Type
  ]]
  def of(
      tpe: dotty.tools.dotc.semanticdb.Type
  ): _root_.dotty.tools.dotc.semanticdb.Annotation =
    _root_.dotty.tools.dotc.semanticdb.Annotation(
      tpe
    )
  // @@protoc_insertion_point(GeneratedMessageCompanion[dotty.tools.dotc.semanticdb.Annotation])
end Annotation
