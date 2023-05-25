// Generated by https://github.com/tanishiking/semanticdb-for-scala3
// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO3

package dotty.tools.dotc.semanticdb
import dotty.tools.dotc.semanticdb.internal.*
import scala.annotation.internal.sharable

@SerialVersionUID(0L)
final case class Diagnostic(
    range: _root_.scala.Option[dotty.tools.dotc.semanticdb.Range] =
      _root_.scala.None,
    severity: dotty.tools.dotc.semanticdb.Diagnostic.Severity =
      dotty.tools.dotc.semanticdb.Diagnostic.Severity.UNKNOWN_SEVERITY,
    message: _root_.scala.Predef.String = ""
) extends SemanticdbGeneratedMessage
    derives CanEqual:
  @transient @sharable
  private[this] var __serializedSizeMemoized: _root_.scala.Int = 0
  private[this] def __computeSerializedSize(): _root_.scala.Int =
    var __size = 0
    if range.isDefined then
      val __value = range.get
      __size += 1 + SemanticdbOutputStream.computeUInt32SizeNoTag(
        __value.serializedSize
      ) + __value.serializedSize;

    {
      val __value = severity.value
      if __value != 0 then
        __size += SemanticdbOutputStream.computeEnumSize(2, __value)
    };

    {
      val __value = message
      if !__value.isEmpty then
        __size += SemanticdbOutputStream.computeStringSize(3, __value)
    };
    __size
  override def serializedSize: _root_.scala.Int =
    var __size = __serializedSizeMemoized
    if __size == 0 then
      __size = __computeSerializedSize() + 1
      __serializedSizeMemoized = __size
    __size - 1

  def writeTo(`_output__`: SemanticdbOutputStream): _root_.scala.Unit =
    range.foreach { __v =>
      val __m = __v
      _output__.writeTag(1, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
    {
      val __v = severity.value
      if __v != 0 then _output__.writeEnum(2, __v)
    };
    {
      val __v = message
      if !__v.isEmpty then _output__.writeString(3, __v)
    };
  def getRange: dotty.tools.dotc.semanticdb.Range =
    range.getOrElse(dotty.tools.dotc.semanticdb.Range.defaultInstance)
  def clearRange: Diagnostic = copy(range = _root_.scala.None)
  def withRange(__v: dotty.tools.dotc.semanticdb.Range): Diagnostic =
    copy(range = Option(__v))
  def withSeverity(
      __v: dotty.tools.dotc.semanticdb.Diagnostic.Severity
  ): Diagnostic = copy(severity = __v)
  def withMessage(__v: _root_.scala.Predef.String): Diagnostic =
    copy(message = __v)

  // @@protoc_insertion_point(GeneratedMessage[dotty.tools.dotc.semanticdb.Diagnostic])
end Diagnostic

object Diagnostic
    extends SemanticdbGeneratedMessageCompanion[
      dotty.tools.dotc.semanticdb.Diagnostic
    ]:
  implicit def messageCompanion: SemanticdbGeneratedMessageCompanion[
    dotty.tools.dotc.semanticdb.Diagnostic
  ] = this
  def parseFrom(
      `_input__`: SemanticdbInputStream
  ): dotty.tools.dotc.semanticdb.Diagnostic =
    var __range: _root_.scala.Option[dotty.tools.dotc.semanticdb.Range] =
      _root_.scala.None
    var __severity: dotty.tools.dotc.semanticdb.Diagnostic.Severity =
      dotty.tools.dotc.semanticdb.Diagnostic.Severity.UNKNOWN_SEVERITY
    var __message: _root_.scala.Predef.String = ""
    var _done__ = false
    while !_done__ do
      val _tag__ = _input__.readTag()
      _tag__ match
        case 0 => _done__ = true
        case 10 =>
          __range = Option(
            __range.fold(
              LiteParser
                .readMessage[dotty.tools.dotc.semanticdb.Range](_input__)
            )(LiteParser.readMessage(_input__, _))
          )
        case 16 =>
          __severity = dotty.tools.dotc.semanticdb.Diagnostic.Severity
            .fromValue(_input__.readEnum())
        case 26 =>
          __message = _input__.readStringRequireUtf8()
        case tag => _input__.skipField(tag)
    dotty.tools.dotc.semanticdb.Diagnostic(
      range = __range,
      severity = __severity,
      message = __message
    )
  end parseFrom

  lazy val defaultInstance = dotty.tools.dotc.semanticdb.Diagnostic(
    range = _root_.scala.None,
    severity = dotty.tools.dotc.semanticdb.Diagnostic.Severity.UNKNOWN_SEVERITY,
    message = ""
  )
  sealed abstract class Severity(val value: _root_.scala.Int)
      extends SemanticdbGeneratedEnum derives CanEqual:
    type EnumType = Severity
    def isUnknownSeverity: _root_.scala.Boolean = false
    def isError: _root_.scala.Boolean = false
    def isWarning: _root_.scala.Boolean = false
    def isInformation: _root_.scala.Boolean = false
    def isHint: _root_.scala.Boolean = false

    final def asRecognized: _root_.scala.Option[
      dotty.tools.dotc.semanticdb.Diagnostic.Severity.Recognized
    ] = if isUnrecognized then _root_.scala.None
    else
      _root_.scala.Some(
        this.asInstanceOf[
          dotty.tools.dotc.semanticdb.Diagnostic.Severity.Recognized
        ]
      )

  object Severity:
    sealed trait Recognized extends Severity

    @SerialVersionUID(0L)
    case object UNKNOWN_SEVERITY extends Severity(0) with Severity.Recognized:
      val index = 0
      val name = "UNKNOWN_SEVERITY"
      override def isUnknownSeverity: _root_.scala.Boolean = true

    @SerialVersionUID(0L)
    case object ERROR extends Severity(1) with Severity.Recognized:
      val index = 1
      val name = "ERROR"
      override def isError: _root_.scala.Boolean = true

    @SerialVersionUID(0L)
    case object WARNING extends Severity(2) with Severity.Recognized:
      val index = 2
      val name = "WARNING"
      override def isWarning: _root_.scala.Boolean = true

    @SerialVersionUID(0L)
    case object INFORMATION extends Severity(3) with Severity.Recognized:
      val index = 3
      val name = "INFORMATION"
      override def isInformation: _root_.scala.Boolean = true

    @SerialVersionUID(0L)
    case object HINT extends Severity(4) with Severity.Recognized:
      val index = 4
      val name = "HINT"
      override def isHint: _root_.scala.Boolean = true

    @SerialVersionUID(0L)
    final case class Unrecognized(unrecognizedValue: _root_.scala.Int)
        extends Severity(unrecognizedValue)
        with SemanticdbUnrecognizedEnum
    lazy val values = scala.collection.immutable.Seq(
      UNKNOWN_SEVERITY,
      ERROR,
      WARNING,
      INFORMATION,
      HINT
    )
    def fromValue(__value: _root_.scala.Int): Severity = __value match
      case 0       => UNKNOWN_SEVERITY
      case 1       => ERROR
      case 2       => WARNING
      case 3       => INFORMATION
      case 4       => HINT
      case __other => Unrecognized(__other)
  end Severity

  final val RANGE_FIELD_NUMBER = 1
  final val SEVERITY_FIELD_NUMBER = 2
  final val MESSAGE_FIELD_NUMBER = 3
  def of(
      range: _root_.scala.Option[dotty.tools.dotc.semanticdb.Range],
      severity: dotty.tools.dotc.semanticdb.Diagnostic.Severity,
      message: _root_.scala.Predef.String
  ): _root_.dotty.tools.dotc.semanticdb.Diagnostic =
    _root_.dotty.tools.dotc.semanticdb.Diagnostic(
      range,
      severity,
      message
    )
  // @@protoc_insertion_point(GeneratedMessageCompanion[dotty.tools.dotc.semanticdb.Diagnostic])
end Diagnostic
