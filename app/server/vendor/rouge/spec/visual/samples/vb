' Copyright (c) 2008 Silken Web - Free BSD License
' All rights reserved.
'
' Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
' * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer
' * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
' * Neither the name of Silken Web nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
'
' THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
' THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
' BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
' GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
' LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
' DAMAGE.

Imports System.Net.Mail
Imports SilkenWeb.Entities
Imports System.Text.RegularExpressions
Imports System.Reflection
Imports SilkenWeb.Validation
Imports System.Globalization
Imports SilkenWeb.Reflection

Namespace SilkenWeb

    ''' <summary>
    ''' Represents an Email and what you can do with it.
    ''' </summary>
    ''' <remarks>
    ''' Keith Jackson
    ''' 11/04/2008
    '''
    ''' This class is intended to be inherrited for providing all manner of system generated emails, each represented by it's own class.
    ''' </remarks>
    Public MustInherit Class EmailBase : Implements IValidatable, IDisposable

#Region " Constants "

        Public Const LenientRegexPattern As String = "\w+([-+.]\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*"
        Public Const StrictRegexPattern As String = "^(([^<>()[\]\\.,;:\s@\""]+(\.[^<>()[\]\\.,;:\s@\""]+)*)|(\"".+\""))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$"
        Public Const InvalidEmailAddressError As String = "The Email address provided was invalid"
        Public Const InvalidEmailAddressErrorWithAddress As String = "The Email address, {0}, provided was invalid"
        Public Const NullEmailAddressError As String = "The Email address was not provided"

#End Region

#Region " Fields "

        Private disposedValue As Boolean

        Private _message As MailMessage = New MailMessage()
        Private _mailClient As SmtpClient

        Private _useStrictValidation As Boolean

#End Region

#Region " Construction "

        ''' <summary>
        ''' Instantiates a new Email of the derived type.
        ''' </summary>
        ''' <param name="sender">The email address of the sender of the message.</param>
        ''' <param name="recipients">The email addresses of the recipients of the message.</param>
        ''' <param name="subject">The subject of the message.</param>
        ''' <param name="body">The body of the message.</param>
        Protected Sub New(ByVal sender As String, ByVal subject As String, ByVal body As String, ByVal ParamArray recipients As String())
            _message.From = New MailAddress(sender)
            For i As Integer = 0 To recipients.Length - 1
                _message.To.Add(recipients(i))
            Next
            _message.Subject = subject
            _message.Body = body
        End Sub

#End Region

#Region " Properties "

        ''' <summary>
        ''' Gets the Attachments for the message.
        ''' </summary>
        Protected Overridable ReadOnly Property Attachments() As AttachmentCollection
            Get
                Return _message.Attachments
            End Get
        End Property

        ''' <summary>
        ''' The email addresses of the BCC recipients of the message.
        ''' </summary>
        Public Property BccRecipients() As String()
            Get
                Return _message.Bcc.ToAddressStringArray()
            End Get
            Set(ByVal value As String())
                _message.Bcc.Clear()
                _message.Bcc.Add(value.ToDelimitedString())
            End Set
        End Property

        ''' <summary>
        ''' The body of the message.
        ''' </summary>
        Protected Overridable Property Body() As String
            Get
                Return _message.Body
            End Get
            Set(ByVal value As String)
                _message.Body = value
            End Set
        End Property

        ''' <summary>
        ''' The email addresses of the CC recipients of the message.
        ''' </summary>
        Public Property CCRecipients() As String()
            Get
                Return _message.CC.ToAddressStringArray()
            End Get
            Set(ByVal value As String())
                _message.CC.Clear()
                _message.CC.Add(value.ToDelimitedString())
            End Set
        End Property

        ''' <summary>
        ''' Gets or Sets a flag to indicate if the body of the message is HTML.
        ''' </summary>
        Public Property IsBodyHtml() As Boolean
            Get
                Return _message.IsBodyHtml
            End Get
            Set(ByVal value As Boolean)
                _message.IsBodyHtml = value
            End Set
        End Property

        ''' <summary>
        ''' Gets the Mail message wrapped by the EmailBase class.
        ''' </summary>
        Protected ReadOnly Property Message() As MailMessage
            Get
                Return _message
            End Get
        End Property

        ''' <summary>
        ''' Gets or Sets the Priority of the message.
        ''' </summary>
        Public Property Priority() As MailPriority
            Get
                Return _message.Priority
            End Get
            Set(ByVal value As MailPriority)
                _message.Priority = value
            End Set
        End Property

        ''' <summary>
        ''' The email addresses of the recipients of the message.
        ''' </summary>
        Public Property Recipients() As String()
            Get
                Return _message.To.ToAddressStringArray()
            End Get
            Set(ByVal value As String())
                _message.To.Clear()
                _message.To.Add(value.ToDelimitedString())
            End Set
        End Property

        ''' <summary>
        ''' The reply email address of the sender of the message.
        ''' </summary>
        Public Property ReplyTo() As String
            Get
                If _message.ReplyTo Is Nothing Then
                    Return String.Empty
                Else
                    Return _message.ReplyTo.Address
                End If
            End Get
            Set(ByVal value As String)
                If _message.ReplyTo Is Nothing Then
                    _message.ReplyTo = New MailAddress(value)
                Else
                    _message.ReplyTo = New MailAddress(value, _message.ReplyTo.DisplayName)
                End If
            End Set
        End Property

        ''' <summary>
        ''' The reply display name of the sender of the message.
        ''' </summary>
        Public Property ReplyToDisplayName() As String
            Get
                If _message.ReplyTo Is Nothing Then
                    Return String.Empty
                Else
                    Return _message.ReplyTo.DisplayName
                End If
            End Get
            Set(ByVal value As String)
                If _message.ReplyTo Is Nothing Then
                    _message.ReplyTo = New MailAddress(_message.From.Address, value)
                Else
                    _message.ReplyTo = New MailAddress(_message.ReplyTo.Address, value)
                End If
            End Set
        End Property

        ''' <summary>
        ''' The email address of the sender of the message.
        ''' </summary>
        Public Overridable Property Sender() As String
            Get
                Return _message.From.Address
            End Get
            Protected Set(ByVal value As String)
                _message.From = New MailAddress(value, _message.From.DisplayName)
            End Set
        End Property

        ''' <summary>
        ''' The display name of the sender of the message.
        ''' </summary>
        Public Overridable Property SenderDisplayName() As String
            Get
                Return _message.From.DisplayName
            End Get
            Protected Set(ByVal value As String)
                _message.From = New MailAddress(_message.From.Address, value)
            End Set
        End Property

        ''' <summary>
        ''' The subject of the message.
        ''' </summary>
        Public Overridable Property Subject() As String
            Get
                Return _message.Subject
            End Get
            Protected Set(ByVal value As String)
                _message.Subject = value
            End Set
        End Property

#End Region

#Region " Methods "

#Region " Send Methods "

        ''' <summary>
        ''' Sends this email
        ''' </summary>
        ''' <param name="mailServer">The SMTP server to use to send the email.</param>
        Public Sub Send(ByVal mailServer As String)
            _mailClient = New SmtpClient(mailServer)
            _mailClient.Send(_message)
        End Sub

        ''' <summary>
        ''' Sends this email asynchronously.
        ''' </summary>
        ''' <param name="mailServer">The SMTP server to use to send the email.</param>
        ''' <param name="userToken">A user defined token passed to the recieving method on completion of the asynchronous task.</param>
        Public Sub SendAsync(ByVal mailServer As String, ByVal userToken As Object)
            _mailClient = New SmtpClient(mailServer)
            _mailClient.SendAsync(_message, userToken)
        End Sub

        ''' <summary>
        ''' Cancels an attempt to send this email asynchronously.
        ''' </summary>
        Public Sub SendAsyncCancel()
            _mailClient.SendAsyncCancel()
        End Sub

#End Region

#End Region

#Region " IValidatable Implementation "

        ''' <summary>
        ''' gets and Sets a flag to indicate whether to use strict validation.
        ''' </summary>
        Public Property UseStrictValidation() As Boolean
            Get
                Return _useStrictValidation
            End Get
            Set(ByVal value As Boolean)
                _useStrictValidation = value
            End Set
        End Property

        ''' <summary>
        ''' Validates this email.
        ''' </summary>
        ''' <returns>A ValidationResponse, containing a flag to indicate if validation was passed and a collection of Property Names and validation errors.</returns>
        Public Function Validate() As ValidationResponse Implements IValidatable.Validate

            Dim retVal As New ValidationResponse()
            Dim mailRegEx As String = If(_useStrictValidation, StrictRegexPattern, LenientRegexPattern)

            ValidateAddress("Sender", retVal, mailRegEx, True)
            ValidateAddresses("Recipients", retVal, mailRegEx, True)
            ValidateAddresses("CcRecipients", retVal, mailRegEx)
            ValidateAddresses("BccRecipients", retVal, mailRegEx)
            ValidateAddress("ReplyTo", retVal, mailRegEx)

            Return retVal

        End Function

        ''' <summary>
        ''' Validates a single Email Address property.
        ''' </summary>
        ''' <param name="propertyName">The name of the property to validate.</param>
        ''' <param name="retVal">The validation response object.</param>
        ''' <param name="mailRegEx">The regular expression pattern to use for validation.</param>
        Private Overloads Sub ValidateAddress(ByVal propertyName As String, ByRef retVal As ValidationResponse, ByVal mailRegEx As String)
            ValidateAddress(propertyName, retVal, mailRegEx, False)
        End Sub

        ''' <summary>
        ''' Validates a single Email Address property.
        ''' </summary>
        ''' <param name="propertyName">The name of the property to validate.</param>
        ''' <param name="retVal">The validation response object.</param>
        ''' <param name="mailRegEx">The regular expression pattern to use for validation.</param>
        ''' <param name="required">Indicates if the address is required; False if not specified.</param>
        Private Overloads Sub ValidateAddress(ByVal propertyName As String, ByRef retVal As ValidationResponse, ByVal mailRegEx As String, ByVal required As Boolean)

            Dim emailAddress As String = ReflectionHelper.Properties.GetProperty(Of String)(Me, propertyName)

            If emailAddress Is Nothing OrElse emailAddress.Length = 0 Then
                If required Then retVal.Add(New KeyValuePair(Of String, String)(propertyName, NullEmailAddressError))
            Else
                If (Not Regex.IsMatch(emailAddress, mailRegEx)) Then
                    retVal.Add(New KeyValuePair(Of String, String)(propertyName, InvalidEmailAddressError))
                End If
            End If

        End Sub

        ''' <summary>
        ''' Validates a string array of Email Address property.
        ''' </summary>
        ''' <param name="propertyName">The name of the property to validate.</param>
        ''' <param name="retVal">The validation response object.</param>
        ''' <param name="mailRegEx">The regular expression pattern to use for validation.</param>
        Private Overloads Sub ValidateAddresses(ByVal propertyName As String, ByRef retVal As ValidationResponse, ByVal mailRegEx As String)
            ValidateAddresses(propertyName, retVal, mailRegEx, False)
        End Sub

        ''' <summary>
        ''' Validates a string array of Email Address property.
        ''' </summary>
        ''' <param name="propertyName">The name of the property to validate.</param>
        ''' <param name="retVal">The validation response object.</param>
        ''' <param name="mailRegEx">The regular expression pattern to use for validation.</param>
        ''' <param name="required">Indicates if the address is required; False if not specified.</param>
        Private Overloads Sub ValidateAddresses(ByVal propertyName As String, ByRef retVal As ValidationResponse, ByVal mailRegEx As String, ByVal required As Boolean)

            Dim emailAddresses() As String = ReflectionHelper.Properties.GetProperty(Of String())(Me, propertyName)

            If emailAddresses Is Nothing OrElse emailAddresses.Length = 0 Then
                If required Then retVal.Add(New KeyValuePair(Of String, String)(propertyName, String.Format(CultureInfo.CurrentCulture, NullEmailAddressError)))
            Else
                For i As Integer = 0 To emailAddresses.Length - 1
                    If (Not Regex.IsMatch(emailAddresses(i), mailRegEx)) Then
                        retVal.Add(New KeyValuePair(Of String, String)(propertyName, String.Format(CultureInfo.CurrentCulture, InvalidEmailAddressErrorWithAddress, emailAddresses(i))))
                    End If
                Next
            End If

        End Sub

#End Region

#Region " IDisposable Implementation "

        Protected Overridable Sub Dispose(ByVal disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                    _message.Dispose()
                End If
                _mailClient = Nothing
                _message = Nothing
            End If
            Me.disposedValue = True
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub

#End Region

    End Class

End Namespace
