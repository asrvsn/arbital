import request from 'request'

const base_url = 'http://' + window.location.hostname + ':5000'

const authenticate = (sessionId) => {
  if (typeof sessionId !== 'string') {
    throw `backend.authenticate: must pass sessionId of type string, got ${sessionId}`
  }

  return {
    get: (path, cb) => {
      return request
        .get({
          url: base_url + path,
          headers: {
            'servant-session-id': sessionId
          }
        }, cb)
    },

    post: (path, payload, cb) => {
      return request
        .post({
          url: base_url + path,
          headers: {
            'servant-session-id': sessionId
          }
        }, cb)
        .json(payload)
    }
  }
}

const post = (path, payload, cb) => {
  return request
    .post(base_url + path, cb)
    .json(payload)
}

export default {
  base_url,
  authenticate,
  post
}


