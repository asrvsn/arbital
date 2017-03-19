import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import { Link } from 'react-router'

import { authSuccess } from '../actions'
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
    const { onAuthSuccess, router } = this.props

    gapi.signin2.render('google-signin-button', {
      onsuccess: (user) => {
        const { id_token } = user.getAuthResponse()
        const url = 'http://' + window.location.hostname + ':5000/login'
        const req = backend.post(
          '/login',
          {tag: 'GoogleTokenAuth', idToken: id_token},
          (err, response, body) => {
            if (err !== null) {
              this._loginErr(err)
            } else {
              if (response.statusCode == 200) {
                this._loginSuccess(body)
              } else {
                this._loginErr(response.statusMessage)
              }
            }
          }
        )
      },
      ...properties.googleSignin
    })
  }

  render() {
    const { isAuthenticated } = this.props
    const { title } = this.state

    return (
      <Dialog
        title={isAuthenticated ? 'You are already logged in.' : title}
        modal={true}
        open={true}
      >
        <Link to='/feed'>asdf</Link>
        <div id='google-signin-button' style={styles.googleSignin} />
      </Dialog>
    )
  }

  _loginSuccess(session) {
    const { onAuthSuccess, router } = this.props
    onAuthSuccess(session)
    router.push('/')
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
