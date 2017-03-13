import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import authSuccess from '../actions'

import Dialog from 'material-ui/Dialog'

class Login extends Component {
  constructor(props) {
    super(props)
    this.state = {
      title: 'Login'
    }
  }

  render() {
    const { isAuthenticated, onAuthSuccess, history } = this.props
    const { title } = this.state

    gapi.signin2.render('google-signin-button', {
      onsuccess: (user) => {
        const { id_token } = user.getAuthResponse()
        const url = window.location.hostname + ':5000/login'
        request
          .post(url)
          .body({tag: 'GoogleTokenAuth', idToken: id_token})
          .on('response', response => {
            if (response.statusCode == 200) {
              onAuthSuccess(JSON.parse(response.body))
              history.push('/')
            } else {
              this.setState({title: 'Login failed: ' + response.body})
            }
          })
      },
    })

    return (
      <Dialog
        title={isAuthenticated ? 'You are already logged in.' : title}
        modal={true}
        open={true}
      >
        <div id='google-signin-button' />
      </Dialog>
    )
  }
}

const mapStateToProps = ({ isAuthenticated }) => ({ isAuthenticated })
const mapDispatchToProps = (dispatch) => ({
  onAuthSuccess: (session) => {
    dispatch(authSuccess(session))
  }
})

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(Login)
