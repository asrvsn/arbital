import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import authSuccess from '../actions'
import backend from '../util/backend'

import Dialog from 'material-ui/Dialog'

const styles = {
  googleSignin: {
    top: 15
  }
}

const properties = {
  googleSignin: {
    scope: 'https://www.googleapis.com/auth/userinfo.profile',
    width: 250,
    height: 50,
    longtitle: true,
  }
}


class Login extends Component {
  constructor(props) {
    super(props)
    this.state = {
      title: 'Login to Arbital'
    }
  }

  componentDidMount() {
    gapi.signin2.render('google-signin-button', {
      onsuccess: (user) => {
        const { id_token } = user.getAuthResponse()
        const url = 'http://' + window.location.hostname + ':5000/login'
        try {
          backend
            .post('/login', {tag: 'GoogleTokenAuth', idToken: id_token})
            .on('response', response => {
              if (response.statusCode == 200) {
                onAuthSuccess(JSON.parse(response.body))
                history.push('/')
              } else {
                this._loginErr(response.statusMessage)
              }
            })
        } catch (e) {
          this._loginErr(e)
        }
      },
      ...properties.googleSignin
    })
  }

  render() {
    const { isAuthenticated, onAuthSuccess, history } = this.props
    const { title } = this.state

    return (
      <Dialog
        title={isAuthenticated ? 'You are already logged in.' : title}
        modal={true}
        open={true}
      >
        <div id='google-signin-button' style={styles.googleSignin} />
      </Dialog>
    )
  }

  _loginErr(err) {
    this.setState({title: 'Login failed: ' + err})
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
