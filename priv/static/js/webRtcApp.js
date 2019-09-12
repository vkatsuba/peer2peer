// --------------------------------------------------------------------------------------------
// API method:
// Opera --> getUserMedia
// Chrome --> webkitGetUserMedia
// Firefox --> mozGetUserMedia
// --------------------------------------------------------------------------------------------

navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia;

// --------------------------------------------------------------------------------------------
// Default Settings
// --------------------------------------------------------------------------------------------

var settings = {
    localVideo: document.getElementById('localVideo'),
    remoteVideo: document.getElementById('remoteVideo'),
    signallingServer: 'wss://localhost:8000/ws',
    iceServers: {"iceServers": [{url:'stun:23.21.150.121'}, {url:'stun:stun.l.google.com:19302'}]},
    mediaParameterAudio: true,
    mediaParameterVideo: true,
    sdpConstraints: {'mandatory': {'OfferToReceiveAudio':true, 'OfferToReceiveVideo':true }},
    constraints: {'audio': true, 'video': {'mandatory': {}, 'optional': []}},
    dtlsConstraints: {"optional": [{"DtlsSrtpKeyAgreement": true}]},
    room: 0,
    initiator: false,
    chatColorLocalText: "#444",
    chatColorRemoteText: "#888",
    msgClassLocal: 'local-msg',
    msgClassRemote: 'remote-msg',
    classLinkToRemote: 'link-to-remote-connect'
};

// --------------------------------------------------------------------------------------------
// Define variables
// --------------------------------------------------------------------------------------------

var isMoz = !! navigator.mozGetUserMedia,
    isWebkit = !! navigator.webkitGetUserMedia,
    isAndroid = navigator.userAgent.toLowerCase().indexOf("android") > -1,
    RTCPeerConnection = window.mozRTCPeerConnection || window.webkitRTCPeerConnection,
    SessionDescription = window.mozRTCSessionDescription || window.RTCSessionDescription,
    IceCandidate = window.mozRTCIceCandidate || window.RTCIceCandidate,
    channel = new WebSocket(settings.signallingServer), // Create channel
    peerConnection = new RTCPeerConnection(settings.iceServers, settings.dtlsConstraints), // Create peer connection
    isRoom = location.search.substring(1, 5) === 'room',
    chatBtn = document.getElementById('chat_submit_btn'),
    inputFiles = document.getElementById('files'),
    sendProgress = document.querySelector('#sendProgress'),
    sendProgressWrap = document.getElementsByClassName('progress'),
    valumeIcon = document.querySelector('.glyphicon-volume-off'),
    videoIcon = document.querySelector('.glyphicon-facetime-video'),
    resizeIcon = document.querySelector('.glyphicon-resize-full'),
    mainBody = document.querySelector('.main'),
    sidebarBody = document.querySelector('.sidebar');
    streamData = [];

// Callback to be called in case of failures...
function errorCallBack(error) {
    console.log("Error callback: " + error);
}

// Attach media stream
function attachMediaStream(element, stream) {
  try {
    element.srcObject = stream;
  } catch (error) {
    element.src = window.URL.createObjectURL(stream);
    video.onloadedmetadata = function(e) {
       video.play();
     };
  }

  streamData.push(stream);
}

// Init channel
function initChannel() {
    channel.onopen = channelOpened;
    channel.onmessage = sendChannelMessage;
    channel.onclose = true; // channelReady

    if (window.File && window.FileReader && window.FileList && window.Blob) {
    } else {
        alert('The File APIs are not fully supported in this browser.');
        return;
    }
}

// Opene channel
function channelOpened() {
    if(isRoom) {
        settings.room = location.search.substring(6);
        sendMessage({'type' : 'ENTERROOM', 'value' : settings.room * 1});
        settings.initiator = true;
    } else {
        sendMessage({'type' : 'GETROOM', 'value' : ''});
        settings.initiator = false;
    }

    // fix for Chromium
    setTimeout(function() {
        navigator.getUserMedia(settings.constraints, onUserMediaSuccess, errorCallBack)
    }, 500);
}

// Send Message
function sendMessage(message) {
    var msgString = JSON.stringify(message);

    channel.send(msgString);
}

// Send channel message
function sendChannelMessage(message) {
    var msgData = message.data,
        msg = JSON.parse(msgData);

    if (msg.type === 'CHATMSG') {
        addChatTxt(msg.value, settings.chatColorRemoteText, settings.msgClassRemote);
    } else if(msg.type === 'CHATMSGFILE'){
        var msgFile = JSON.parse(msg.value);

        onFileReceived(msgFile.name, msgFile.size, msgFile.data, settings.chatColorRemoteText, settings.msgClassRemote);
    } else if (msg.type === 'offer') {
        peerConnection.setRemoteDescription(new SessionDescription(msg));
        peerConnection.createAnswer(setLocalAndSendMessage, errorCallBack, settings.sdpConstraints);
    } else if (msg.type === 'answer') {
        peerConnection.setRemoteDescription(new SessionDescription(msg));
    } else if (msg.type === 'candidate') {
        var candidate = new IceCandidate({sdpMLineIndex:msg.label, candidate:msg.candidate});
        peerConnection.addIceCandidate(candidate);
    } else if (msg.type === 'GETROOM') {
        settings.room = msg.value;
        onRoomReceived(settings.room);
    } else if (msg.type === 'WRONGROOM') {
        window.location.href = "/";
    }
}

// Do call if user media success
function onUserMediaSuccess(stream) {
    attachMediaStream(settings.localVideo, stream);

    peerConnection.onicecandidate = onIceCandidate;
    peerConnection.onaddstream = onRemoteStreamAdded;

    peerConnection.addStream(stream);

    if (settings.initiator) {
        doCall()
    }
}

// Send message to ice candidate
function onIceCandidate(event) {
    if (event.candidate){
        sendMessage({'type': 'candidate', 'label': event.candidate.sdpMLineIndex, 'id': event.candidate.sdpMid, 'candidate': event.candidate.candidate});
    }
}

// Remote stream
function onRemoteStreamAdded(event) {
    attachMediaStream(settings.remoteVideo, event.stream);
}

// Create offer for constraints
function doCall() {
    var constraints = mergeConstraints(settings.sdpConstraints);

    peerConnection.createOffer(setLocalAndSendMessage, errorCallBack, constraints);
}

// Do merge for constraints
function mergeConstraints(cons) {
    var constraints = {'optional': [], 'mandatory': {'MozDontOfferDataChannel': true}};

    if (!isMoz){
        for (var prop in constraints.mandatory) {
            if (prop.indexOf('Moz') != -1) {
                delete constraints.mandatory[prop];
            }
        }
    }

    for (var name in cons.mandatory) {
        constraints.mandatory[name] = cons.mandatory[name];
    }

    constraints.optional.concat(cons.optional);

    return constraints;
}

// Set local and send message
function setLocalAndSendMessage(sessionDescription) {
    peerConnection.setLocalDescription(sessionDescription);
    sendMessage(sessionDescription);
}

// Send message to chat
function sendChatMsg(e) {
    var msgline = document.getElementById('chat_input'),
        msg = msgline.value;

    msgline.value = '';

    if(msg.length){
        addChatTxt(msg, settings.chatColorLocalText, settings.msgClassLocal);
        sendMessage({"type" : "CHATMSG", "value" : msg});
    }
}

// --------------------------------------------------------------------------------------------
// Chat send message API
// --------------------------------------------------------------------------------------------

// Receive room
function onRoomReceived(room) {
    var p = document.createElement('p'),
        a = document.createElement('a'),
        text = document.createTextNode('Now, if somebody wants to join you, should use this link: '),
        url = window.location.href + "?room=" + room;

    a.setAttribute('href', url);
    a.innerHTML  = url;
    p.appendChild(text);
    p.appendChild(a);

    addChatTxt(p.innerHTML, settings.chatColorRemoteText, settings.classLinkToRemote, true);
}

// Receive file
function onFileReceived(name, size, data, chatColor, msgClass) {
    var p = document.createElement('p'),
        a = document.createElement('a'),
        text = document.createTextNode('Download file: '),
        endText = document.createTextNode(' bytes'),
        msgClass = msgClass ? msgClass : '';

    a.setAttribute('href', data);
    a.setAttribute('download', name);
    a.innerHTML = name;
    p.appendChild(text);
    p.appendChild(a);
    p.appendChild(endText);

    addChatTxt(p.innerHTML, chatColor, msgClass, true);
}

// Show message in DOM
function addChatTxt(msg, msgColor, msgClass, innerHTML) {
    var chatArea = document.getElementById('chat_area'),
        divMsgWrap = document.createElement('div'),
        divMsg = document.createElement('div'),
        divTime = document.createElement('div'),
        msgClass = msgClass ? msgClass : 'font',
        timeClass = msgClass ? msgClass + ' time' : 'time',
        innerHTML = innerHTML ? innerHTML : false;

    divMsgWrap.setAttribute('class', 'msg-wrap');
    divMsgWrap.setAttribute('color', msgColor);

    divTime.setAttribute('class', timeClass);
    divTime.innerHTML = getTime();

    divMsg.setAttribute('class', msgClass);
    if (innerHTML) {
        divMsg.innerHTML = msg;
    } else {
        divMsg.innerText = msg;
    }

    divMsgWrap.appendChild(divTime);
    divMsgWrap.appendChild(divMsg);

    chatArea.appendChild(divMsgWrap);

    scrollToMsg();
}

// Get time
function getTime() {
    var date = new Date(),
        hours = date.getHours(),
        minutes = date.getMinutes();

    hours = hours < 10 ? '0' + hours : hours;
    minutes = minutes < 10 ? '0' + minutes : minutes;

    return hours + ':' + minutes;
}

// Scroll to last message
function scrollToMsg(){
    var chatArea = document.getElementById('chat_area'),
        offsetHeight = chatArea.scrollHeight;

    chatArea.scrollTop = offsetHeight;
}

// Send file
function sendFileMsg(event){
    var filelist = this.files;

    if(filelist.length){
        for (var i = 0; filelist.length > i; i++) {
            var fileLoadStatus = document.getElementById('file_load_status'),
                file = filelist[0],
                chunkSize = 16384,
                receiveBuffer = [];

            var sliceFile = function(offset) {
                var reader = new window.FileReader(),
                    slice = [],
                    percent = 0;

                reader.onload = (function() {
                    return function(e) {
                        receiveBuffer.push(e.target.result);

                        if (file.size > offset + e.target.result.byteLength) {
                          window.setTimeout(sliceFile, 0, offset + chunkSize);
                        } else{
                            var received = new window.Blob(receiveBuffer);

                            try {
                              var blobURL = received;
                            } catch (error) {
                              var blobURL = window.URL.createObjectURL(received);
                            }

                            msg = JSON.stringify({"type" : "file", "name" : file.name, "size" : file.size, "data" : blobURL});

                            sendMessage({"type" : "CHATMSGFILE", "value" : msg});
                            onFileReceived(file.name, file.size, blobURL, settings.chatColorLocalText, settings.msgClassLocal);
                        }
                        // See in DOM send progress
                        // Get percent
                        percent = ((offset + e.target.result.byteLength) * 100) / file.size;
                        sendProgress.setAttribute('style', 'width: ' + percent + '%;');
                        sendProgressWrap[0].classList.remove('hidden');

                        // Clear the process load bar
                        setTimeout(function() {
                            if(percent === 100) {
                                sendProgress.setAttribute('style', 'width: 0%;');
                                sendProgressWrap[0].classList.add('hidden');
                            }
                        }, 500);
                    };
                })(file);

                slice = file.slice(offset, offset + chunkSize);
                reader.readAsArrayBuffer(slice); // here we can use just: reader.readAsArrayBuffer(file)
            };

            sliceFile(0);
        }
    }
}

// Toggle audio stream
function toggleAudio() {
    streamData[0].getAudioTracks()[0].enabled = !(streamData[0].getAudioTracks()[0].enabled);
    this.classList.toggle('red');
}

// Toggle video stream
function toggleVideo() {
    streamData[0].getVideoTracks()[0].enabled = !(streamData[0].getVideoTracks()[0].enabled);
    this.classList.toggle('red');
}

// Toggle resize
function toggleResize() {
    mainBody.classList.toggle('col-md-9');
    mainBody.classList.toggle('col-md-12');
    sidebarBody.classList.toggle('hidden');
    this.classList.toggle('red');
}

// hook click to enter button
function hookEnter(event) {
    if(event.keyCode === 13 && !event.shiftKey) {
        event.preventDefault();

        sendChatMsg();
    }
}

// --------------------------------------------------------------------------------------------
// Add event listener
// --------------------------------------------------------------------------------------------

// Send message to chat
chatBtn.addEventListener('click', sendChatMsg, false);

// hook enter button
document.addEventListener('keydown', hookEnter, false);

// Chancge text in DOM after file upload
inputFiles.addEventListener('change', sendFileMsg, false);

// Toggle audio
valumeIcon.addEventListener('click', toggleAudio, false);

// Toggle video
videoIcon.addEventListener('click', toggleVideo, false);

// Toggle resize
resizeIcon.addEventListener('click', toggleResize, false);

// --------------------------------------------------------------------------------------------
// Init functions
// --------------------------------------------------------------------------------------------

// Init channel
initChannel();
