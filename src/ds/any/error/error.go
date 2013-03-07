package ds_any_error

type (
	ErrorObject struct {
		message   string
		irritants []interface{}
	}
)

func (self ErrorObject) Error() string {
	return self.message
}

func (self ErrorObject) Message() string {
	return self.message
}

func (self ErrorObject) Irritants() []interface{} {
	return self.irritants
}
