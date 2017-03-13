import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import Dialog from 'material-ui/Dialog'

const Login = (props) => {
  const { isAuthenticated, onAuthSuccess } = props

  gapi.signin2.render('google-signin-button', {
    onsuccess: (user) => {
      const { id_token } = user.getAuthResponse()
      const url = window.location.hostname + ':5000/login'
      onAuthSuccess(id_token)
    },
  })

  return (
    <Dialog
      title={isAuthenticated ? 'You are already logged in.' : 'Login'}
      modal={true}
      open={true}
    >

    </Dialog>
  )
}

